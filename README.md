# Ceres

A news tweet and article collector.

## Usage

A running Mongodb instance is needed.

Configurate server (see '/opt/example-server-config.edn' for reference) and start server with

```
lein run opt/server-config.edn
```

## Docker

Build it
```
sudo docker build --rm -t kordano/ceres .
```

Install and run [dockerfile/mongodb](https://index.docker.io/u/dockerfile/mongodb/ "dockerfile/mongodb") if not installed
```
sudo docker pull dockerfile/mongodb 
sudo docker run -d -p 27017:27017 --name mongodb dockerfile/mongodb
```

Fill in twitter credentials and other server configuration (e.g. port, build, ...) in `opt/server-config.edn` on the local machine where docker is running. Be sure that you are the only one having access to this file. **Do not** share this file with others.


Run it either directly without any shared volumes with
```
sudo docker run -d --link mongodb:db --name ceres -p 8082:8082 kordano/ceres
```
or define a shared volume in `opt/deploy-ceres` for backup and log-files and run it
```
sudo sh ./opt/deploy-ceres
```

## License

Copyright © 2014 Konrad Kühne

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
