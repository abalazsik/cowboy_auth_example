-module(appuser).

-export([new/3, getUsername/1, getPassword/1, getRoles/1, hashPassword/1]).

%%appuser object

-record(appuser,{
    username = [] :: string(),
    password = [] :: string(),
    roles = [] :: [atom()]
}).

new(Username, Password, Roles) ->
    #appuser{
        username = Username,
        password = hashPassword(Password),
        roles = Roles
    }.

getUsername(User) when is_record(User, appuser) ->
    User#appuser.username.

getPassword(User) when is_record(User, appuser) ->
    User#appuser.password.

getRoles(User) when is_record(User, appuser) ->
    User#appuser.roles.

-spec hashPassword(string()) -> binary().

hashPassword(Password) when is_list(Password) ->
    base64:encode(crypto:hash(sha256,Password)).
