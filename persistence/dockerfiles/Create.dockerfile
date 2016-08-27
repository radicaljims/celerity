FROM postgres:9.5

EXPOSE 5555

ENV POSTGRES_USER valhalla
ENV POSTGRES_PASSWORD valhalla
ENV POSTGRES_DB valhalla

ADD init/init.sql /docker-entrypoint-initdb.d/10-init.sql
ADD init/auth.sql /docker-entrypoint-initdb.d/20-auth.sql
ADD init/user_management.sql /docker-entrypoint-initdb.d/30-user-management.sql