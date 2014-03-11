% jabber.dbc.mtview.ca.us

override_acls.

{http_server, 					"http://123.178.27.74"}.
{http_server_service_core, 		"/pet/core/request"}.
{http_server_service_client, 	"/pet/client/request"}.

{loglevel,5}.

{odbc_server, {mysql, "localhost", "ejabberd", "root", "123456"}}.

{acl, admin, {user, "liangc", "liangc.com"}}.

{registration_timeout, infinity}.

{access, announce,    [{allow,  admin},
                       {deny,   all}]}.
{access, c2s,         [{deny,   blocked},
                       {allow,  all}]}.
{access, c2s_shaper,  [{none,   admin},
                       {normal, all}]}.
{access, configure,   [
				{allow,  admin},
                {deny,   all}
]}.

{access, disco_admin, [{allow,  admin},
                       {deny,   all}]}.
{access, muc_admin,   [{allow,  admin},
                       {deny,   all}]}.
{access, register,    [{allow,  all}]}.
{access, s2s_shaper,  [{fast,   all}]}.


{auth_method, 			external}. 
{hosts, 				["liangc.com","test.com","rycdb"]}.
{outgoing_s2s_port,     5269}.
{shaper, normal,       {maxrate, 1000}}.
{shaper, fast,         {maxrate, 50000}}.
{welcome_message,       none}.

{listen,
 [
  {5222, ejabberd_c2s, [
                        {access, c2s},
                        {shaper, c2s_shaper},
                        starttls, {certfile, "/etc/ejabberd/server.pem"},
                        {max_stanza_size, 65536}
                       ]},
  {5223, ejabberd_c2s, [
                        {access, c2s},
                        {shaper, c2s_shaper},
                        tls, {certfile, "/etc/ejabberd/server.pem"},
                        {max_stanza_size, 65536}
                       ]},
  {{5269, "::"}, ejabberd_s2s_in, [
                                   {shaper, s2s_shaper},
                                   {max_stanza_size, 131072}
                                  ]},
  {5280, ejabberd_http, [
	                        http_bind,
 	                        http_poll,
	                        web_admin
                     ]}
 ]
}.

{s2s_use_starttls, true}.
{s2s_certfile, "/etc/ejabberd/server.pem"}.



{modules, [
           {mod_register,  []},
           {mod_roster,    []},
           {mod_privacy,   []},
           {mod_configure, []},
           {mod_disco,     []},
           {mod_stats,     []},
           {mod_vcard,     []},
           {mod_offline,   []},
           {mod_echo,      [{host, "echo.jabber.dbc.mtview.ca.us"}]},
           {mod_private,   []},
%          {mod_irc,       []},
           {mod_muc,       []},
           {mod_pubsub,    []},
           {mod_time,      []},
           {mod_last,      []},
           {mod_version,   []}
          ]}.


% Local Variables:
% mode: erlang
% End:
