### Valhalla, Docker-remix

Some infrastructure for building and running a Postgres container. ~~Might one day include postgrest~~.

*Now with 100% more ```postgrest```!*

### Posting JSON documents

The version of postgrest I'm using is a little peculiar.

Assuming we have a table that looks like this:

```sql
CREATE TABLE fsevents (id serial PRIMARY KEY, data jsonb);
```

To POST something into that with postgrest you have to format the payload like such:

```json
[{
    "data": {"add":"/hi.there"}
    
}]
```

Note that we're specifying the column to write the JSON into, and we've wrapped the object in a list.

See this issue: https://github.com/begriffs/postgrest/issues/682

### Instructions

1. Be on a Docker host
2. Run ```./run.sh```
3. Run ```./docker-psql.sh``` to enter the Postgres container
4. Type ```\d``` and you should see something like
    ```
    Password for user valhalla:
    psql (9.5.3)
    Type "help" for help.

    valhalla=# \d
    List of relations
    Schema |     Name      |   Type   |  Owner
    --------+---------------+----------+----------
    public | greets        | table    | valhalla
    public | greets_id_seq | sequence | valhalla
    (2 rows)
    ```

5. (Ctrl-d will exit you from psql)
6. (```./stop.sh``` will stop the containers)
7. (```docker-compose rm``` will delete the containers)

By default the Postgres container will be running on port 5555 (peek at ```./scripts/docker-psql.sh```).

Note that something seems broken with Postgres' 9.6 image. Couldn't get POSTGRES_USER and friends to work!

### PostgREST

```docker-compose``` will also start-up a *PostgREST* container you can use to interact with Postgres:

1. Run ```curl localhost:3000``` and observe

    ```
    [{"schema":"public","name":"greets","insertable":true}]
    ```
