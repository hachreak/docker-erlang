-module(create_tables).

-export([init_tables/0, insert_user/3, insert_project/2,
         get_user_id/1]).

-include_lib("stdlib/include/qlc.hrl").

-record(user, {id, name}).
-record(project, {title, description}).
-record(contributor, {user_id, project_title}).

% How init the database.
%
% $> erl -setcookie mycookie -mnesia dir '"/tmp/mnesia_store"' -sname mynode
% (mynode@b2223ccac07d) 1> mnesia:create_schema([node()]).
% (mynode@b2223ccac07d) 2> mnesia:start().
% (mynode@b2223ccac07d) 3> c(create_tables).
% (mynode@b2223ccac07d) 4> create_tables:init_tables().
% (mynode@b2223ccac07d) 5> mnesia:info().
% ---> Processes holding locks <---
% ---> Processes waiting for locks <---
% ---> Participant transactions <---
% ---> Coordinator transactions <---
% ---> Uncertain transactions <---
% ---> Active tables <---
% contributor    : with 0        records occupying 300      words of mem
% project        : with 0        records occupying 300      words of mem
% user           : with 0        records occupying 300      words of mem
% schema         : with 4        records occupying 758      words of mem
% ===> System info in version "4.7", debug level = none <===
% opt_disc. Directory "/tmp/mnesia_store" is used.
% use fallback at restart = false
% running db nodes   = [mynode@b2223ccac07d]
% stopped db nodes   = []
% master node tables = []
% remote             = []
% ram_copies         = [contributor,project,user]
% disc_copies        = [schema]
% disc_only_copies   = []
% [{mynode2@b2223ccac07d,disc_copies}] = [schema]
% [{mynode2@b2223ccac07d,ram_copies}] = [user,project,contributor]
% 5 transactions committed, 0 aborted, 0 restarted, 6 logged to disc
% 0 held locks, 0 in queue; 0 local transactions, 0 remote
% 0 transactions waits for other nodes: []
% ok
% (mynode@b2223ccac07d) 6> create_tables:insert_project(myproject, "hello worlds project").
% ok
% (mynode@b2223ccac07d) 7> create_tables:insert_user(1, pippo, [myproject]).
% {atomic,ok}
% (mynode@b2223ccac07d) 8> mnesia:dirty_read(contributor, 1).
% [{contributor,1,myproject}]
% (mynode@b2223ccac07d) 9> create_tables:get_user_id(pippo).
% {atomic,[1]}
% (mynode@b2223ccac07d) 10>
%
%
% Storage types: ram_copy, disc_copy (ram and disc) or disc_only_copy.
%
% Table types:
%  - set: means that keys, by default the first field in the table
%    definition record, must be unique. if a key is inserted into the table
%    with the same key, it'll overwrite the previously inserted record.
%  - ordered_set: stores all the elements within the tables ordered on their
%    keys. It's not supported for disc_only_copy.
%  - bag: it allows the table to store records for which the keys are not
%    unique. The records themeselves must be different in some way but the
%    keys can be repeated. Records that are complete clones of other records
%    will be stored once.

init_tables() ->
  % create_table: name and definition.
  % with record_info/2 we extract the names of the fields from the record.
  % default properties:
  %  - read/write
  %  - type ram_copies (save the records in ram)
  %  - load priorities of 0 (lowest)
  %  - only records of the same name as the table name can be stored within
  %  - it's of type "set" meaning keys must be unique
  %  - loca_content is set to false
  mnesia:create_table(user,
                      [{attributes, record_info(fields, user)}]),
  mnesia:create_table(project,
                      [{attributes, record_info(fields, project)}]),
  mnesia:create_table(contributor,
                      [{type, bag},
                       {attributes, record_info(fields, contributor)}]).

insert_user(Id, Name, ProjectTitles) when ProjectTitles =/= [] ->
  User = #user{id = Id, name = Name},
  Fun = fun() ->
            % start transaction and write record in user table
            mnesia:write(User),
            % and then a list of record on contributor table for each project
            lists:foreach(
              fun(ProjectTitle) ->
                  % check if project exists on table
                  [#project{title = ProjectTitle}] = mnesia:read(project, ProjectTitle),
                  % if it's true, than write new record on contributor table
                  mnesia:write(#contributor{user_id = Id, project_title = ProjectTitle})
              end,
              ProjectTitles
             )
        end,
  % the function Fun executes within the contect o a Mnesia transaction.
  %  1. it's executed as a unit, all operation either succed or the whole transaction fails.
  %  2. it's capable of locking information in the db, so that concurrent operations on the
  %     db don't negatively impact one to another.
  % the use of transactions is critical for ensuring the integrity of the db accross complex
  % operations.
  mnesia:transaction(Fun).

insert_project(Title, Description) ->
  % dirty operations don't respect transactions or locks on the database.
  mnesia:dirty_write(#project{title = Title, description = Description}).

% query example with qlc: SELECT user.id FROM user WHERE user.name = Name
get_user_id(Name) ->
  mnesia:transaction(
    fun() ->
       Q = qlc:q([U#user.id || U <- mnesia:table(user), U#user.name == Name]),
       qlc:e(Q)
    end
  ).
