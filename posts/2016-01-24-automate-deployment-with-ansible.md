---
title: Automate deployment with Ansible
image: /images/ansible.png
thumbnail: /images/thumbnail-ansible.png
---

It's been a while since I wrote my last blog post. Today, I want to share with you my small experience with [Ansible](http://www.ansible.com/). Ansible is an open source project for application deployment. I heard of it last year but never took the time to dig in. Last week, I gave it a new try for [Quantic Telecom](https://www.quantic-telecom.net) to deploy new containers easily and I really enjoy it!

<!--more-->

### Why using Ansible?

![Ansible is simple IT automation](/images/ansible.png)

There is a lot of reasons to use Ansible for your deployments or any other tool like [Puppet](https://puppetlabs.com/) or [Chef](https://www.chef.io/chef/). In my specific use case, I needed to install a lot of new services, each one on a private container: [Piwik Analytics](https://piwik.org/), [OwnCloud](https://owncloud.org/), [Cachet HQ](https://cachethq.io/), [Gitlab](https://about.gitlab.com/), etc (yes, I'm a big fan of open-source software!). At first sight, nothing in common with all these services, there are Ruby and PHP, Nginx and Apache, Debian packages and manual installs. But in fact, for all these containers, I needed to configure the timezone, secure SSH, add some users and private keys, configure ZSH…

In fact, as soon as you start having a documentation explaining how to create and configure new machines with some code to copy paste, I think you need to look into some deployment tools like Ansible.

### Why I like Ansible

Ansible is state-driven, it means that everything in your scripts will be link to state (if you do your job correctly). For example, instead of saying: "I want to install texlive-full", you will say: "I want the package texlive-full to be installed". It changes everything, this feature allows you to run your scripts multiple times without to worry about bad changes. The first time Ansible will check if texlive-full is installed and will install it if it's not. Then, the next times, it will show you that everything is already good to go. Moreover, it's really fast to run.

![latex-full is already installed, two "ok"s, no "changed"](/images/ansible-ok.jpg)

The second thing I like about Ansible is the big number of [Core Modules](http://docs.ansible.com/ansible/modules_core.html). Core Modules are really easy to use and can do almost every simple system administration task with a cool syntax, sometimes they're even easier to remember than the Linux commands.
```yaml
---

- name: be sure user thibaud exists
  user: name=thibaud shell=/bin/zsh state=present

- name: be sure thibaud's public key is in authorized_keys
  authorized_key:
    user: "thibaud"
    key: "{{ lookup('file', 'thibaud.pub') }}"
    state: present
```

In this example, I use the "user" Core Module and the "authorized_key" Core Module. The module documentation is really clear and you have access to every parameters and a lot of examples to understand how to use it. If I change one single thing manually on a server, Ansible will correct the problem. It's important to name things, and with this capture, I realize that my naming for the first task is not really accurate and should be: "be sure user thibaud exists and use ZSH".

![thibaud is using bash for the staging server, one "ok", one "changed"](/images/ansible-changed.jpg)

Last thing important with Ansible, it's only text so you can easily use Git to keep version control. We use a lot Proxmox and templates for containers and it's hard to keep track of the changes for several hundreds MBs images. With Ansible, everything can be saved within a few kBs.

### What I dislike with Ansible

The first time I open the documentation, I had trouble finding a good way to start. By default, Ansible read a weird `/etc/ansible` for the roles. I don't know who is using this and why but, for me, it shouldn't be the default. Everyone should have an ansible directory with their roles and their hosts and Ansible should throw an error if it can't detect any of this.

I also try to use roles build by the community, and I found two kinds of roles : the easy ones and the over engineered ones. Due to the lack of package manager, I preferred to code the easy ones from scratch, because it's easier to manage than to have to add in the documentation to download these roles. And for the complex ones, I had troubles make them work with my system so I just develop my owns.

### Conclusion

Go get [Ansible](http://www.ansible.com/)! :-)
```bash
pacman -S ansible
```
