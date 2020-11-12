#!/bin/bash

docker run --name nginx -u $(id -u):$(id -g) -p 80:8080 -v $(pwd)/nginx.conf:/etc/nginx/nginx.conf -v $(pwd):/usr/share/nginx/html:ro -d nginx
