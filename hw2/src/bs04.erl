-module(bs04).
-export([decode/2 ]).

-record(jsonkv, {list = <<>>, key=[], value =[], raw = <<>>,flag}).


decode(Bin,Selector) ->

	case Selector of
		
		%proplist -> decode_in_proplist(Bin,<<>>);   
		
		map ->  json(#jsonkv{raw = Bin, flag = key},<<>>,#{})  

	end.

json(RAW ,_, Map) when RAW#jsonkv.raw == <<>> ->
       Map;    

json(RAW, KeyVal, Map)  ->
       <<H/utf8,Rest/binary>> = RAW#jsonkv.raw,
       %io:format([H]),  
  case  H of
      
  	 "{"  ->  json(#jsonkv{raw=Rest},<<>>,Map),  io:format("${") ; %запускаем новый
  	 "'"  ->  json(RAW#jsonkv{raw=Rest},KeyVal,Map);%кавычку игнорируем 
     
     ","  ->   case RAW#jsonkv.flag of
            key ->  json(RAW#jsonkv{raw=Rest, key = KeyVal, flag = val, value = <<>>},<<>>,Map), io:format([KeyVal]);
            list ->json(RAW#jsonkv{raw=Rest},<<KeyVal,H>>,Map);
            val ->  json(RAW#jsonkv{raw=Rest, key = <<>>, flag = key, value = <<>>},<<>>,#{RAW#jsonkv.key => KeyVal})
             end;                 
                                           % добавляем следующий ключ 
     ":"  ->  json (RAW#jsonkv{raw = Rest,flag = val,key = KeyVal},<<>>,Map); % ищем значение 
     "}"  ->  case RAW#jsonkv.flag of
            key ->  json(RAW#jsonkv{raw=Rest, key = KeyVal, flag = val, value = <<>>},<<>>,Map),io:format(Map);
            list ->json(RAW#jsonkv{raw=Rest},<<KeyVal,H>>,Map), io:format(Map);
            val ->  json(RAW#jsonkv{raw=Rest, key = <<>>, flag = val, value = <<>>},<<>>,#{RAW#jsonkv.key => KeyVal}), io:format(Map)
             end;         % ищем значение 
   	 H   ->  json(RAW#jsonkv{raw = Rest}, <<KeyVal/binary,H/utf8>>,Map)%, io:format([KeyVal])  %Добавляем символ к ключу 
    
     
  end.

 

