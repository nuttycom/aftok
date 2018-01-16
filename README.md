Aftok - A New Kind of Company
=============================

Let's talk about an idealized model of commerce: Make something that you think
other people will value. The difference between the cost of making that thing
and the price you sell it for is the amount of value that your effort has
created in the world. Divide this value by the number of hours it took you to
produce the thing, and you arrive at a good estimate for the value of your
time.

This idea of how the exchange of value should take place in an economy is
appealingly simple, and almost completely unrelated to how most of us
experience our economic participation. For the vast majority of us, the
relationship between the value that we produce and the amount we earn is almost
entirely opaque. The effect of this is that we instead use the relative demand
for our skills as a proxy for this value, but this proxy is subject to a great
deal of distortion. In the standard model, it is in the interest of every
corporation to pay less (on average) for its employees' work than the
value that they generate, so that the remaining profits may accrue to the
corporate body itself, and its principal officers.

An aftok is a new, experimental company structure that proposes a different approach.

The Essentials
--------------

An aftok is a group of individuals, working collaboratively to create a good or
service for which they wish to be paid, who distribute revenue among themselves
according to the relative amount of time each person has spent contributing to
the project. As soon as any revenue is received, it is instantaneously paid out
to the contributors in proportion to their contributions.

You'll note that nowhere in this description is there any entity equivalent to
a corporation; there is no fictional entity that owns resources or employs the
contributors. [http://aftok.com](http://aftok.com) is a service that allows
customers to compensate the creators of a product or service directly without
the need for a corporate middleman. As such, it is *revenue*, not profit, that is
divided among the collaborators. That distribution is performed by a simple
algorithm which ensures that all contributors are treated fairly.

As was mentioned above, the baseline metric used to determine what share of
revenue is distributed to each contributor is based upon the amount of time
spent on the project, so we provide a simple time tracking service that can be
used by contributors to log the time that they spend working on the project.
Any hour (really, any second, but hours are a bit easier to think about) logged
is equivalent to any other hour logged by any contributor that ends at the same
moment; at a first approximation, this implies that the every individual's life
is considered to be of equal value. 

In order to allow for differences in skill to be compensated at different
rates, we provide a mechanism that allows any contributor to compensate their
peers for their work, by either "tithing" some of their share of revenue to
their collaborators, or by allocating time that they are working to another's
account. Revenue tithes can be one-time-only occurrences, or can be scheduled
to recur whenever revenue is received. Tithes of time, however, are permanent;
once worked time has been logged to a contributor's address, it is treated as
though that contributor had been working for that period and cannot be changed.
In a traditional corporation, these sorts of tithes are made implicitly,
usually to management and stockholders, and making them is a precondition of
accepting employment. By contrast, in an aftok a market for talent
can evolve within a group, and all such tithes are explicit, public, and at
the sole discretion of the individuals making them. If someone wants to claim
that their time is worth a premium, all that they have to do is convince their
collaborators. 

One problem that arises from the algorithm as stated up to this point is that,
over time, it would become economically impossible for new collaborators to
join a company, because the accrued time of earlier contributors would vastly
outweigh any new time contributed, and thus the new collaborator could never
reach parity with the rest of the company when revenue is distributed. For this
reason, the value of an hour contributed is not constant. Instead, the value of
an hour decays according to a tunable function of time elapsed since the hour
was logged. For example, after 6 months from the time of the original
contribution, an hour contributed may begin to depreciate at a rate of 2
minutes per month, such that after 5 years the value of the hour has been
completely exhausted and when revenue is distributed that hour will no longer
be compensated. Of course, each contributor's time-share of the company's
revenue is also subject to constant dilution as more contributions are
continuously being made. 

In addition to the practical reasons for time depreciation, there is an
additional, more philosophical point to be made, which is that contributions
made in the distant past do not justify compensation in perpetuity. Companies,
and the services they provide, must change over time in order to stay relevant
and competitive. On a long enough timescale we're all manufacturers of buggy
whips and steam engineers; while people in these professions created value long
ago, in the present day their contributions have been rendered irrelevant by
progress. 

At this point it's important to point out that, even though a collaborator may
cease contributing to a project (or may have forked the project, or been forked
away from by the other collaborators, a process that will be described
later), the compensation for their contribution up to that point does not
immediately go to zero, as it would be the case when someone stops getting paid
when they leave a traditional corporation; instead, their logged time is paid
out according to the ordinary schedule, with the combined forces of dilution
and depreciation eating away at the share of revenue they receive. The reason for
this is that, while they may no longer be permitted to contribute new work, the
value that they produced in the past must still be recognized and compensated.
For more information on this situation, see the 'Irreconcilable Differences'
section.

Making Decisions
----------------

Just as the depreciated amount of time that a person has devoted to the company
(relative to that devoted by the rest of the collaborators) determines what
proportion of the revenue of the company they are awarded, it also determines
the amount of influence that person has in making decisions that affect the
company as a whole. The [http://aftok.com](http://aftok.com) platform provides
a voting service that can be used by collaborators to make collective
decisions.

The voting system provided uses a range-voting model where the ratings chosen
by an individual for the options provided are weighted by their revenue
distribution percentage at the ending moment of the voting period. Only those
contributors who are currently permitted to record time to the company's logs
are allowed to vote; while former contributors are entitled to compensation
to the value that they created, they do not have the right to influence the
company's future.

Shared Resources
----------------

As was alluded to earlier, in an aftok, there is no central entity that can be
responsible for owning property, so one of the first problems that arises is
how to obtain shared resources that are needed by the company to do its
business. Ownership of things is a privilege that should be reserved for real
flesh-and-blood people, so any resource that is needed should be either
individually owned, or, if used by all, then rented.

In order to provide an equitable means for raising money to pay for a rented
resource from among the collaborators of a company, the approach offered by
this service is that units of time (the primary unit of account within a
company) are *auctioned* to raise the money. The fundamental idea is that if
someone is contributing money to purchase a shared resource, obviously some
effort of theirs was required in the past for them to obtain the money that
they are contributing, and so in some sense the contribution of money is
equivalent to a contribution of some amount of their time. The purpose of the
auction is to determine what amount of time their monetary contribution is
worth.

The process goes like this:

First, a vote must be held to determine the amount of money to be raised in a
shared resource auction; an auction involves the allocation of new logged units
of time and consequently dilutionary pressure that will be felt by all the
collaborators in the company, so it is important to obtain the consent of the
members before proceeding. In addition, a vote may be held to determine a
designee who will be responsible for renting or otherwise handling the
acquisition of the shared resource once the auction is complete.

If the vote passes (if a nonzero amount is selected by the weighted range vote)
then a Dutch auction opens. Each collaborator may then place bids, where a bid
consists of both a monetary amount and the amount of time that they expect to
receive in exchange if they win.

When the auction closes, bids are sorted in descending order by the ratio of
currency to time expressed by each bid. Then, the top bids are accepted until
the amount of money that the auction seeks to raise has been reached. Each
winning bidder will be awarded the amount of time specified in their bid upon
payment of the monetary amount of the bid to the auction's designee. The result
of this auction process is that the dilution collectively suffered by members
of the company is minimized.

There is, of course, nothing preventing a subset of collaborators from making
an external agreement for shared ownership of a resource that is then rented to
the company; however, the particulars of such agreements are at present outside
the scope of what is addressed by the [http://aftok.com](http://aftok.com)
platform.

Irreconcilable Differences
--------------------------

In any group of people, there may come a point where someone, or some subset of
the group, decides that they simply cannot continue to work with the remainder.
In a traditional corporation, people can be fired. In an aftok, it's not quite
so simple. Fortunately, the past couple of decades of experience of the
open-source software world have provided us with an excellent model for how to
deal with such a situation when there's no fixed hierarchy of control: the
"fork."

In software, forking a project involves taking the current state of the source
code, and creating a competing project using that source as a base. The aftok
structure permits an analogous process. 

When a schism arises, and there is no possible reconciliation the entire
company may split into two or more subsets of the original collaborators. In
the case of such a fork, the parent company must essentially dissolve, but this
process is facilitated by the fact that the company owns no property, and any
leases of shared resources may be termintated and reestablished by the child
companies. At the point of a fork, the log that tracks the time of each
contributor is duplicated, and two or more new companies are formed such that
each new child company may begin extending their logs independently. As each
child company begins taking in revenue, it is distributed according to the
same rules as before the split; it is even possible, for example, for an 
individual to contribute their time to more than one child company that arises
from the fork. Over time, the new contributions on either side of the fork
will dilute the claims of those who are no longer contributing, and the
depreciation process will finally reduce their interest in received revenue
to nothing.

A group may, of course, choose to fork away from a single contributor; this is
analogous to firing someone from a traditional corporation. Such a person 
however, always has the option to form a full fork on their own and become
a competing company.

Aftok Philosophy
================

The idea of the aftok arose out of my experience in the open-source community.
Something we have learned from the past couple of decades of experimentation
with open-source projects is that a group of motivated individuals, working in
their own individual interest in a framework of collaboration and trust, can
achieve amazing things.

The modern corporation is essentially feudal in structure; a hierarchy of lords
to whom the greatest benefits accrue, lieutenants who provide command and
control, and serfs, whose labor fuels the enterprise but who may never achieve
the level of financial independence of the upper classes. The lore of modern
business is that those at the top of these hierarchies are justly compensated
for providing the greatest contribution or taking the most significant risks
but I believe this justification is contrived to mitigate the natural
dissatisfaction that people feel when confronted with inequality; a simpler
explanation of their popularity is that hierarchical structures are
relatively stable. However, as the open-source software development world has
shown us, hierarchies are not the only kind of organizational structures that
can lead to the creation of great things.

Open-source development, however, has a couple of problems. While it's true
that a great deal of open-source work is paid for by traditional corporations,
it is actually remarkably difficult to make a living as an independent
contributor to an open-source project. The aftok is designed as minimal
structure that is needed for a group of individuals to collaborate on a project
for which they hope to be paid. 

More importantly, however, the aftok company structure is designed to
maximize individual freedom within the context of performing paid,
collaborative work. 

The Value of Time
-----------------

What is the value of an hour of your life? This is a question that I've spent
years wrestling with while trying to balance time spent with my family and
friends, time spent working, and time alone with my thoughts. In my consulting
life I've assigned monetary value to my hours somewhat arbitrarily; I determine
an hour to be worth enough to maintain a certain lifestyle and yet not be so
much that nobody will be willing to pay it. Yet, the question has always
remained with me: what is the actual value, in terms of revenue, of the effort
of the hours that I've put in? 

A related question is, how does the value of my time genuinely compare to the 
value of the time of others? The premise that an hour of my life is somehow more
or less valuable than an hour of someone else's seems a little problematic;
we don't know how long we'll live. What is your last hour worth, or your first?

The only answer that I can come up with is this. We should begin with an idea
of equality. However, it's undeniable that the value one person can create
within a given period of time will differ from the value created by another, and
a mechanism must be provided to allow this difference to be recognized and
compensated. Since there is no central authority to make compensation decisions, 
it is up to the individual contributors to honestly assess and compensate their
peers. This brings us to the next, and perhaps most important, point.

The Value of Trust
------------------

The aftok concept grew out of a simple question: what would be the best way to
structure a for-profit organization in which all of the members of that
organization trusted one another? 

Many of the mechanisms which corporations use to limit the potential impact of
malicious actors also unavoidably act to inhibit individual creativity and
productivity. Hierarchies of control can ensure that outcomes desired by those
at the top are achieved, even when those goals are poor or shortsighted. The
aftok ideal seeks another way.

As has been mentioned before, open-source software projects have demonstrated
that a group of motivated and skilled individuals working toward a common goal
in an environment of shared trust requires no, and indeed is inhibited by, a
hierarchy of control. If you feel that you can trust your collaborators, you
should be able to trust their judgment as to what they should be working on,
and that their perspective, while perhaps distinct from yours, is as valid as
your own. If you don't trust someone to this degree, you simply should not work
with them; if you choose to work with someone whom you feel that you may need
to control, you're setting yourself up for failure anyway. 

Wherever trust is inhibited in a business, whether by secrecy (of salary
information, for example) hierarchy of control (with the threat of firing or
punishment available as a goad) or even lack of access (can you really
interrupt your CEO whenever you want?) it encourages people to behave in
cynical, rather than enlightened, self-interest. This cynicism is the sort that
causes people to reserve their best work for projects where they have the
freedom of self-determination. 

Given this, when working in the context of an aftok, any question related to
how you should behave with respect to others in your company comes down to a
simple question: do you trust them or not? If you trust them, then trust their
judgment and in their good intentions; there is no need to attempt to control
them, only perhaps to convince them or find common ground when you disagree. 

It's possible, of course, that you'll be wrong. That you've misplaced your
trust, and that you'll have to change your mind and fork away from them. This
risk is not unique to an aftok. Sometimes, there can even be people whom you
trust and even admire greatly, but just don't want to work with, and this is
okay. A virtue of the aftok structure is that the damage that can be done by an
incompetent or even malicious actor is limited by the very fact that there is
no centralized entity that can own assets, or even control revenue in any but
the most temporary fashion. Fraud is possible on a limited scale (someone could
overstate the hours that they've worked), but this situation is equally likely
to occur in a traditional corporation, and the scale upon which fraud of other
sorts can be perpetrated is greatly reduced.

How Things Should Work
----------------------

Given all of this, it should be obvious that (Aftok.com)[http://aftok.com] is
itself being built by an aftok, rather than some ordinary corporate entity. As
such, I'm now going to slip into first-person for a moment to express my
personal motivation for initiating this project. My name is Kris Nuttycombe,
and I'm a software engineer. In the previous several years, I've been
exceptionally fortunate in that I've been able to work with some of the
smartest and most self-motivated software development teams in the world.
However, that work has always been done in the context of traditional
organizations, and as such I've always been a little bit dissatisfied with how
the dynamics of hierarchical control have impacted the products that I've 
been a part of creating. 

My objective in creating this service is simple; I want to be able to support
my family doing the work that I love, in an environment of mutual respect and
trust with my collaborators. I firmly believe that I will personally achieve
greatest success in this endeavor if I reject entirely notions of control and
coercion.  Communication is only possible between equals <sup>1</sup>, and I
believe that where communication is inhibited, the end result suffers. As such,
it's up to each of my collaborators to decide for themselves what work, if any,
they wish to do in the creation and promotion of this service. Each of them
knows far better than I what value he or she is able to contribute. The
structure described here is a mere skeleton, and software is never complete
until it is abandoned.  However, I hope that what I've created thus far is
sufficient to make a start, and it is up to all of us, working together, to
determine what we may ultimately achieve. 

