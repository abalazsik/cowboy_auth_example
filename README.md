cowboy_auth_example
=====

Cowboy Authentication & Authorization Example

In this example you can see:
- login + authentication
- (self-made) session handling
- authorization on dynamic content
- authorization on static content

Run
-----

    $ rebar3 run

Go to http://localhost:8000 and log in

The application has hardcoded users and roles.

| Username    | Password | Roles                 |
|-------------|----------|-----------------------|
| simpleuser1 | password | role1                 |
| simpleuser2 | password | role1, role2          |
| manager     | password | role1, role2, manager |

Now, you can visit page respectively to the user's roles.

Pages

| Page        | Who can view        |
|-------------|---------------------|
| /login.html | anybody             |
| /role2.html | role2 users         |
| /           | authenticated users |
| /managers   | manager users       |

