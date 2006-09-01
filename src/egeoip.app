{application,egeoip,
 [{description,"geolocation by IP"},
  {vsn,"0.01"},
  {modules,[egeoip, egeoip_app, egeoip_sup]},
  {registered, []},
  {env, [{dbfile, city}]},
  {mod, {egeoip_app, []}},
  {applications,[kernel,stdlib]}]}.
