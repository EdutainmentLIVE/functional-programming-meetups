# Elm and Pusher Pub/Sub Example

This mini application was live-coded to demonstrated a basic Elm application
including javascript interop. To demonstrate, you will need to create a Pusher
application at `https://pusher.com` and update `assets/index.html` and
`backend/src/Lib.hs' with the appropriate Pusher credentials. Then run
`docker-compose up` in this directory and open two browsers to
`http://localhost:8000`. Messages posted from one browser window will appear in
the other.
