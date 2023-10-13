db_ets_erl
=====

An OTP application

Build
-----

    $ rebar3 compile

For convinience there is also docker-compose file together with configuration
to set up Grafana with Prometheus datasource.

To start detached containers:
```bash
docker-compose up -d
```

To stop containers and remove volumes:
```bash
docker-compose down -v
```

To view Grafana interface go to: `localhost:3000` and log in with following credentials
- username: admin
- password: grafana

To view Prometheus dashboard go to: `localhost:9090`
