apt-get update

apt-get install nginx --assume-yes

service nginx restart

# TODO: figure out server_name in nginx.cnf

mv /etc/nginx/nginx.conf /etc/nginx/nginx.conf.backup
mv /vagrant/conf/nginx.conf /etc/nginx

nginx -s reload

# /vagrant/bin/server # possible to get game server running not as root?
