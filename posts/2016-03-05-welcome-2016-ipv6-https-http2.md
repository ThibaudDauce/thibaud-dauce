---
title: "Welcome 2016: IPv6, HTTPS & HTTP/2.0"
image: /images/https.png
thumbnail: /images/thumbnail-https.png
---

It's been a few weeks since a set a AAAA record for my domain name and allow IPv6 to visit my website. Same with SSL and Let's Encrypt. Today I switched from Apache 2 to Nginx and enabled HTTP/2.0 on my server.

<!--more-->

### IPv6

I'm the co-founder of Quantic Telecom, an operator and ISP for student in Rouen, France and I can tell you, IPv4 is dead! We really need IPv6, and not 3 years from now, today! So, change your VPS provider if you don't have an IPv6, set your AAAA DNS records and listen it:
```
listen [::]:443;
```

![IPv6 is the most recent version of the Internet Protocol](/images/ipv6.png)

### HTTPS everywhere

#### Generate certificates

With [Let's Encrypt](https://letsencrypt.org/), certificates are now free for everyone. So no excuse, just set up HTTPS (and HTTPS only). I first use the Let's Encrypt Python script but today I switch to an unofficial bash implementation of the free (as in free speech) Let's Encrypt protocol: [Neilpang/le](https://github.com/Neilpang/le). If you want to set up HTTPS on your server, just type:
```bash
git clone https://github.com/Neilpang/le.git
cd le
./le.sh install
# reset terminal
le issue /var/www/html/ thibaud.dauce.fr thibaud-dauce.fr,www.thibaud-dauce.fr ec-384
le installcert thibaud.dauce.fr /etc/nginx/cert.pem  /etc/nginx/cert.key /etc/nginx/ca.crt "cp /etc/nginx/cert.pem /etc/nginx/fullchain.pem && cat /etc/nginx/ca.crt >> /etc/nginx/fullchain.pem && service nginx reload"
```

#### Use them with Nginx

![Nging webserver](/images/nginx.svg)

```bash
ssl_certificate         /etc/nginx/cert.pem;
ssl_certificate_key     /etc/nginx/cert.key;
ssl_client_certificate  /etc/nginx/ca.crt;
```

#### Secure SSL

```bash
ssl_protocols              TLSv1 TLSv1.1 TLSv1.2;
ssl_prefer_server_ciphers  on;
ssl_ciphers                "EECDH+AES:+AES128:+AES256:+SHA";

# HSTS
add_header Strict-Transport-Security "max-age=31536000; includeSubDomains";
```

#### Why did I switch from Apache?

![Apache is losing the web to Nginx ([source](http://www.nextplatform.com/2016/02/24/how-apache-is-losing-the-web-to-nginx/))](/images/nginx-apache.jpg)

The length of the configuration files are similar but I prefer the Nginx JSON-like syntax over Apache XML. I'm sure there is a lot of errors in my configuration files (I'm new to Nginx) so, if you find something to change, please ping me on [Twitter](https://twitter.com/ThibaudDauce).

```bash
# /etc/nginx/conf.d/default.conf
server {
    listen       80;
    listen       [::]:80;
    server_name  thibaud.dauce.fr;
    return       301 https://$server_name$request_uri;
}
```

```bash
# /etc/nginx/conf.d/default-ssl.conf
server {
    ssl on;
    ssl_certificate         /etc/nginx/fullchain.pem;
    ssl_certificate_key     /etc/nginx/cert.key;

    # SSL security
    ssl_protocols              TLSv1 TLSv1.1 TLSv1.2;
    ssl_ciphers                "EECDH+AES:+AES128:+AES256:+SHA";

    ssl_dhparam /etc/ssl/certs/dhparam.pem;
    ssl_ecdh_curve secp384r1;

    ssl_prefer_server_ciphers  on;
    ssl_session_cache          shared:SSL:10m;

    # HSTS
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains";

    listen 443       ssl http2 default_server;
    listen [::]:443  ssl http2 default_server;
    server_name      thibaud.dauce.fr;

    location / {
        root /var/www/html;
    }
}
```

To use this config file, you need to generate a stronger DHE parameter (it's gonna take a while ^^):
```bash
cd /etc/ssl/certs
openssl dhparam -out dhparam.pem 4096
```

But then you should get an A+ [on every existing SSL test](https://tls.imirhil.fr/https/thibaud.dauce.fr)!

![CryptCheck](/images/cryptcheck.png)

### HTTP/2.0

As you can see in the previous config files, I simply add `http2` at the end of my `listen` line. It's really just that with Nginx 1.9. If you run a Debian 8 as I do, add these deb repositories to get the last version:
```bash
# /etc/apt/sources.list.d/nginx.list
deb http://nginx.org/packages/mainline/debian/ jessie nginx
deb-src http://nginx.org/packages/mainline/debian/ jessie nginx
```

And then:
```bash
wget http://nginx.org/keys/nginx_signing.key
apt-key add nginx_signing.key
apt-get update
apt-get upgrade
```

<h2 style="text-align: center;">IPv6, HTTPS & HTTP/2.0</h2>
<h2 style="text-align: center;">Welcome 2016!</h2>

Thanks [Aeris](https://twitter.com/aeris22) for your help, check [his french blog post](https://blog.imirhil.fr/2015/09/02/cryptcheck-verifiez-implementations-tls.html) about SSL security.
