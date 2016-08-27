# docker stop postgrest ; docker stop postgres-9.5
# docker rm postgrest   ; docker rm postgres-9.5
docker-compose up --build -d

docker ps | grep postgres
