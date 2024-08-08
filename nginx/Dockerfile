FROM nginx

RUN apt -y update && apt install -y nginx && apt install openssl -y
RUN openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout /etc/nginx/server.key -out /etc/nginx/server.crt -subj "/C=US/ST=CA/L=SanFrancisco/O=MyOrg/OU=MyUnit/CN=localhost"

COPY ./nginx.conf /etc/nginx/nginx.conf