%% Technique/Code borrowed form mochiweb source
-module(itty_mime).
-export([from_extension/1, guess_mime/1]).

from_extension(".html") ->
    "text/html";
from_extension(".css") ->
    "text/css";
from_extension(".js") ->
    "application/x-javascript";
from_extension(".jpg") ->
    "image/jpeg";
from_extension(".gif") ->
    "image/gif";
from_extension(".png") ->
    "image/png";
from_extension(_) ->
    undefined.

guess_mime(File) ->
    case from_extension(filename:extension(File)) of
	undefined ->
	    "text/plain";
	Mime ->
	    Mime
    end.
