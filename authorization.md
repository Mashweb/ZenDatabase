# Authentication and Authorization of the Zen Database, version 0.1

The alpha version of zen-db currently exposes all its entries
to the whole internet.
The addition of a pair of 'fields' in a zen-db entry will tell zen-db
to which users access to the entry should be granted.
These 'fields' will be just a prepended list of two integers.
The first integer will be a user id, the second a group id
(to refer to a set of users).
If an authenticated user's user id or group id matches that of an entry,
access to that entry will be granted.
A group id equal to 0 (zero) will indicate the entry is shared with the world.

The zen-db will check authentication, user id, and group id
before responding to a request.
Someday it might be nice to add read and write fields for user and read and
write fields for group to specify read and write access to each entry.
Authentication implies that access to zen-db must be restricted to HTTPS
protocol. It also implies that there must be some kind of login to zen-db
or that the end user's login credentials must somehow be shared securely
with zen-db.
