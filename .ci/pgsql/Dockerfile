ARG BUILD_FROM=postgres:11
FROM ${BUILD_FROM}
ARG POSTGRES_USER=postgres
COPY --chown=$POSTGRES_USER pg_hba.conf /var/lib/postgresql/pg_hba.conf
COPY --chown=$POSTGRES_USER server.key /var/lib/postgresql/server.key
COPY --chown=$POSTGRES_USER server.crt /var/lib/postgresql/server.crt
COPY --chown=$POSTGRES_USER root.crt /var/lib/postgresql/root.crt
RUN chmod 600 /var/lib/postgresql/pg_hba.conf
RUN chmod 600 /var/lib/postgresql/server.key
RUN chmod 600 /var/lib/postgresql/server.crt
RUN chmod 600 /var/lib/postgresql/root.crt
EXPOSE 5432