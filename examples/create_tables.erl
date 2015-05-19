-module(create_tables).

-export([init_tables/0]).

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
