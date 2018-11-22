---
title: Live without Flash
image: /images/html5.png
thumbnail: /images/thumbnail-html5.png
---

Adobe Flash is awful. There are a lot of bugs and security problems with this software. Today, HTML5 is a good alternative but it requires a bit of setup. Some websites still have video players in Flash and it's impossible to watch without.

What can we do?

<!--more-->

![The future is HTML5](/images/html5.png)

### First, Firefox configuration

Firefox is the best browser, not because its performances are better, but because Gecko is an alternative to Webkit (the layout engine software component for rendering web pages in Chrome, Chromium, Safari and Opera). If we don't want Google to be the new Internet Explorer, we need to fight for diversity in rendering engines.

How run HTML5 videos in Firefox? First, we need to check some configuration. YouTube has a perfect web page for this task: [Youtube HTML5](https://www.youtube.com/html5).

To enable all the possibilities of HTML5, you need to use the default HTML5 player. Then to be able to watch 480p and 1080p videos, there are a few steps.

* [Media Source Extensions.](http://www.ghacks.net/2014/05/10/enable-media-source-extensions-firefox/) tldr; go to `about:config` and switch to true `media.mediasource.enabled`.
* [MSE & H.264.](http://www.ghacks.net/2014/07/25/enable-mse-h2-64-support-youtube-firefox-right-now/) tldr; go to `about:config` and switch to true `media.fragmented-mp4.*` except `media.fragmented-mp4.use-blank-decoder`.
* [MSE & WebM VP9](https://www.youtube.com/watch?v=R4No4kv3TA8) tldr; go to `about:config` and switch to true `media.mediasource.webm.enabled`.

With Arch Linux, I think you should install `gst-plugins-good` and `gst-libav` too.

Now you can watch HTML5 videos with Firefox in every website.

### Then some trick for old Flash websites

I use two CLI tools to watch Flash videos: `youtube-dl` and `livestreamer`.

#### youtube-dl

To install `youtube-dl`, visit [their website](https://rg3.github.io/youtube-dl/). Or if you're using Arch Linux, just run `yaourt -S youtube-dl`. This little piece of software allows you to download YouTube videos but not only. You can list all available websites with `youtube-dl --list-extractors`. To use it:
```bash
youtube-dl link-to-your-video
```

You can also download YouTube play lists, pages, etc. There are a lot of options like `--write-all-thumbnails` or `--write-subs`. Just check the man page, everything is explained.

#### Livestreamer

Same as `youtube-dl`, for the installation, just go to [their website](https://github.com/chrippa/livestreamer). With Arch Linux, run `yaourt -S livestreamer`. The problem with `youtube-dl` is you can't watch the video being downloaded because it downloads the video and the audio separately. With `livestreamer` you can stream a video from a lot of websites (`livestreamer --plugins`). To use it:
```bash
livestreamer -p vlc link-to-your-video best
```

I used a lot `livestreamer` with my old computer because VLC is way better than HTML5 to decode 1080p videos. I also use it with Twitch streams.

#### Last trick

I often watch AlloCiné, a french website which provides information on French cinema. They have a Flash video player and neither `youtube-dl` nor `livestreamer` support it. With the plugin Greasemonkey on Firefox, I use the script [AlloCine_Zap](http://userscripts-mirror.org/scripts/show/59373) to watch videos as HTML5 (and remove adds ^^).


----
**Edit: thanks to Christophe Cluizel (Awesome guy! Go follow him [\@CCluizel](https://twitter.com/CCluizel) for some Scala, Spark and Machine Learning stuff), there's now a solution for "MSE & WebM VP9". It's not working on my computer but it may work for you. Thanks for his contribution.**
