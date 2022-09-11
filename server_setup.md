# Server Setup 

## Basic Info

- Contablo
- IP: 194.163.151.152
- Users: root, wn

## Set up

Run in this order

### Update and upgrade

As root

```
apt upgrade
apt full-update
apt autoremove
```

### Add certificates

As root

```
mkdir /root/.ssh && cd /root/.ssh
nano authorized_keys
# copy in key avoiding line breaks
```

### Create user

As root

```
adduser wn
usermod -aG sudo wn
rsync --archive --chown=wn:wn ~/.ssh /home/wn
```

### Enable firewall

As root

```
ufw app list
## check OpenSSH is available
ufw allow OpenSSH
ufw enable
ufw status ## check OpenSSH is allowed
```

## Setup nginx

### Install

As wn

```
sudo apt update
sudo apt install nginx
sudo ufw app list ## check Nginx options added
sudo ufw allow 'Nginx HTTP' ## since no certificate
sudo ufw status ## check enabled
```
http:// of ip address should show welcome page

### Fix hash block issue?

```
sudo nano /etc/nginx/nginx.conf
```

Find the server_names_hash_bucket_size directive and remove the # symbol to uncomment the line.

### Server block

As wn

Create directories and initial html

```
sudo mkdir -p /var/www/flood.waternumbers.com/html
sudo chown -R $USER:$USER /var/www/flood.waternumbers.com/html
sudo chmod -R 755 /var/www/flood.waternumbers.com
nano /var/www/flood.waternumbers.com/html/index.html
```

and copy in 

```
<html>
    <head>
        <title>Welcome to flood.waternumbers.com!</title>
    </head>
    <body>
        <h1>Success!  The flood.waternumbers.com server block is working!</h1>
    </body>
</html>
```

Create server block

```
sudo nano /etc/nginx/sites-available/flood.waternumbers.com
```

and copy in

```
server {
        listen 80;
        listen [::]:80;

        root /var/www/flood.waternumbers.com/html;
        index index.html index.htm index.nginx-debian.html;

        server_name flood.waternumbers.com www.flood.waternumbers.com;

        location / {
                try_files $uri $uri/ =404;
        }
}
```

Link server block

```
sudo ln -s /etc/nginx/sites-available/flood.waternumbers.com /etc/nginx/sites-enabled/
```

test

```
sudo nginx -t
```

restart

```sudo systemctl restart nginx```

### SSL certificate

Install certbot
```
sudo apt install python3-certbot-nginx
```

Run to get certificates

```sudo certbot --nginx```

Change firewall settings
```
sudo ufw allow 'Nginx Full'
sudo ufw delete allow 'Nginx HTTP'
```

Check auto renewal

```
sudo certbot renew --dry-run
```

## set up R 

- Install as from the CRAN wenpage

- pkgconf libcurl4-openssl-dev libsodium-dev zlib1g-dev
