%%% HTTP Request
-record(http_request,
	{requestor_ip,
	 http_path,
	 http_method,
	 http_ver_maj,
	 http_ver_min,
	 client_identity="-",
	 client_username="-"
	 }).
