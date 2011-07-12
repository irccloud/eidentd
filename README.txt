Erlang identd server.

call eidentd_registry:set_ident/5 to register connections
call eidentd_registry:ident/3 to look up
 
automatically starts identd listener on any:20113

use application envs to configure
