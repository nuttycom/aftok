Aftok
=====

> *"I have to figure out how to balance the work I can make a living on with the 
> work I can’t, because the work I can’t make a living on is more important."*
>     -- [Meredith L. Patterson](https://medium.com/message/how-i-explained-heartbleed-to-my-therapist-4c1dbcbe1099)

It's impossible to make a living writing open-source software. Certainly, there
are exceptions; a few companies eke out a relatively spare existence selling
"enterprise support" licenses for free software, and lots of people employed by
private companies contribute to open-source products on time paid for by their
employer. Nonetheless, a great deal of the open-source software that developed
is a labor of love. OSS developers are artists, but they're fortunate that
their art is in a medium that also, at present, makes it easy enough to find
paid work that few of us have to starve while making our art.

Unlike many other arts (excepting music) open-source software development is 
frequently a deeply collaborative endeavor. 

* Trust cannot be imposed, it can only be grown.
* Last section needs to be more positive, deemphasize risk.

(12:36:54 PM) nuttycom1: Reiterating my earlier question, this time via StackOverflow: http://stackoverflow.com/questions/30466275/http-basic-auth-in-snap
(12:41:03 PM) dmj`: nuttycom1: why not use snap's cookie-based auth?
(12:41:33 PM) carter_cloud: mightybyte:  lpsmith  did that posgres-simple snaplet transaction bug ever get fixed?
(12:41:40 PM) nuttycom1: dmj`: my clients aren't necessarily browsers.
(12:41:55 PM) nuttycom1: In fact, they're mostly not browsers.
(12:43:17 PM) dmj`: nuttycom1: are they mobile clients?
(12:43:31 PM) nuttycom1: dmj`: no, other servers.
(12:48:21 PM) dmj`: nuttycom1: basic auth isn't secure, you're sending the password (base-64 encoded) on every request, it could be fine if it's all over https. But still, a token based system would be more secure. Are these severs internal / external? (i.e. yours or someone elses)
(12:53:16 PM) nuttycom1: dmj`: Token-based systems are no more secure if they're not over https, but that's a side point; in my experience it's common in any case to use the Authorization headers to carry authentication tokens so that they don't have to pollute the request payload in other ways. 
(12:54:14 PM) nuttycom1: I have a bunch of CLI tools that access my services via curl and such, which prompt for auth credentials along the way. Using Basic is convenient for these kinds of tools.
(12:54:49 PM) nuttycom1: All the servers *are* in my control, however. I suppose I could use an X-My-Auth header or some such, but I don't much see the point.
(12:55:38 PM) nuttycom1: At least, they're under my control at the moment, but obviously the API I'm providing is ultimately going to be accessible to anyone on the open web, so I might as well make things easy for them.
(12:59:00 PM) mightybyte: carter_cloud: Yes
(12:59:32 PM) mightybyte: http://blog.melding-monads.com/2015/02/12/announcing-snaplet-postgresql-simple-0-6/
(01:00:42 PM) dmj`: nuttycom1: token based systems only transmit the credentials on the initialize request to generate the token, not on every request, so not sure how it's just as secure. It seems like setting up your own oauth provider would be ideal in this case, especially if you plan on releasing this api to the public. If your servers are not exposed to the outside world, why even have them authenticate against each other, if you're using A
(01:00:43 PM) dmj`: you using a VPC? 
(01:00:49 PM) dmj`: initial*
(01:00:52 PM) mightybyte: carter_cloud: I guess that post never hit reddit.
(01:02:17 PM) mightybyte: nuttycom1: I don't know of a basic auth package for snap (probably in part due to the concerns dmj` has pointed out), but I wouldn't expect that it would be too hard to write.
(01:02:42 PM) nuttycom1: mightybyte: Hah.... yeah, I provide one as context in my SO question.
(01:04:49 PM) carter_cloud: mightybyte: btw Stephen Diehl has a neat prototype of a streaming Postgres binding https://github.com/elsen-trading/pgstream
(01:07:49 PM) nuttycom1: dmj`: fair enough; I suppose that I can add token handling to the CLI tools, it's just a little more work since I'll have to encrypt the token before it's stored locally, and require the user to decrypt on each request rather than provide credentials. That's probably the right way to go.
(01:07:51 PM) dmj`: nuttycom1: I can't speak for other token-based systems, but json web tokens are typically encrpyted (HMAC SHA-256), basic auth data is just encoded. So your last line of defense in basic auth is ssl, and your surface area (every request) is much larger. I dunno, if you think basic auth suits your needs then go for it, but it sounds to me like you should become an oauth provider here.
(01:08:16 PM) mightybyte: nuttycom1: responded
(01:09:10 PM) nuttycom1: mightybyte: thanks!
(01:10:45 PM) dmj`: nuttycom1: yea, you can create your own key server! Then create tcp connections from your key server to all other api servers. You should change the key somewhat frequently (This secret key is used to hash all tokens). So your key server can broadcast the new secret to all servers via tcp, then (if you're using haskell and not that node.js single-threaded stuff) you could migrate everyones tokens over to the new key transparen
(01:10:45 PM) dmj`: if new key has been sent, then rehash with it, and put it in the header).
(01:11:16 PM) dmj`: only one-thread, I don't know how people do it
(01:11:20 PM) dmj`: one thread*
(01:12:18 PM) nuttycom1: Interesting, thanks dmj`. Looks like I have some additional investigation to do.
