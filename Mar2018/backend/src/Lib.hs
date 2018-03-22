module Lib
  ( runServer
  ) where

import            Control.Monad.IO.Class (liftIO)
import            Data.Aeson
import qualified  Data.ByteString.Char8 as BS
import qualified  Data.ByteString.Lazy as LBS
import qualified  Data.Text as Text
import qualified  Data.Text.Lazy as LazyText
import qualified  Data.Text.Encoding as Enc
import            Happstack.Server
import            Network.Pusher (Pusher)
import qualified  Network.Pusher as Pusher

{-
 Fill these credentials in with your own Pusher creds to allow
 the frontend to authenticate and push events.
 -}
pusherCreds :: Pusher.Credentials
pusherCreds =
  Pusher.Credentials
    { Pusher.credentialsAppID     = 111111
    , Pusher.credentialsAppKey    = BS.pack "1234567890abcdef1234"
    , Pusher.credentialsAppSecret = BS.pack "4321cedcba0987654321"
    , Pusher.credentialsCluster   = Just (Pusher.Cluster (Text.pack "us2"))
    }

runServer :: IO ()
runServer = do
  pusher <- Pusher.getPusher pusherCreds
  putStrLn "Connected to pusher"
  simpleHTTP nullConf (routes pusher)

routes :: Pusher -> ServerPart Response
routes pusher =
  mconcat
  [ dirs "pusher/auth" auth
  , assets
  ]

{-
 auth implements the endpoint required to authenticate
 pusher clients on the frontend. This endpoint is called
 by pusher library when a client subscribes to a private
 channel. Pusher requires clients to be authenicate for
 pushing events directly from the browser.
 -}
auth :: ServerPart Response
auth = do
  decodeBody (defaultBodyPolicy "/tmp" 4096 4096 4096)

  socketID <- LazyText.toStrict <$> lookText "socket_id"
  channelName <- LazyText.toStrict <$> lookText "channel_name"

  let sig = Pusher.authenticatePrivate
              pusherCreds
              socketID
              (Pusher.parseChannel channelName)

  ok $ toResponse (AuthResponse sig)

{-
 assets serves any static files found in the assets directory
 of the project. The assets directly includes the index.html
 file and the build output from the elm-make script.
 -}
assets :: ServerPart Response
assets =
  serveDirectory DisableBrowsing ["index.html"] "/elm-pub-sub/assets"

channel :: Pusher.Channel
channel =
  Pusher.Channel
    { Pusher.channelType = Pusher.Private
    , Pusher.channelName = Text.pack "messages"
    }

data AuthRequest =
  AuthRequest
    { authSocketID :: Pusher.SocketID
    , authChannelName :: Pusher.ChannelName
    }

newtype AuthResponse  = AuthResponse BS.ByteString

instance FromJSON AuthRequest where
  parseJSON =
    withObject "AuthRequest" $ \o ->
      AuthRequest
        <$> (o .: Text.pack "socket_id")
        <*> (o .: Text.pack "channel_name")

instance ToJSON AuthResponse where
  toJSON (AuthResponse sig) =
    object [ Text.pack "auth" .= Enc.decodeUtf8 sig ]

instance ToMessage AuthResponse where
  toMessage = encode
  toContentType _ = BS.pack "application/json"

