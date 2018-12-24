-module(user_service).

-export([authenticateUser/2, getUser/1]).

%%this is the user database
getUser("simpleuser1") -> appuser:new("simpleuser1","password",[role1]);
getUser("simpleuser2") -> appuser:new("simpleuser2","password",[role1, role2]);
getUser("manager") -> appuser:new("manager","password",[manager, role1, role2]).

authenticateUser(undefined, _) ->
    false;
authenticateUser(_, undefined) ->
    false;
authenticateUser(Username, Password) when is_list(Username) andalso is_list(Password) ->
    User = getUser(Username),

    case (appuser:getPassword(User) == appuser:hashPassword(Password)) of
        true -> {true, User};
        false -> false
    end.

