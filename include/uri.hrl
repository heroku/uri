-record(uri, {scheme :: 'undefined' | uri:scheme(),
              userauth :: 'undefined' | uri:userauth(),
              host :: 'undefined' | uri:host(),
              port = default :: 'default' | inet:port_number(),
              path :: 'undefined' | uri:path(),
              q :: 'undefined' | uri:querystring(),
              frag :: 'undefined' | uri:frag()
             }).
