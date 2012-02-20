-module(protobuf).
-export([parse_varint/1, parse_message/1, encode_signed/2, decode_signed/1]).

% encode_signed/decode_signed - encode and decode signed ints

%encode_signed(Int, Bits)
encode_signed(Int, 32) -> (Int bsl 1) bxor (Int bsr (31))
;
encode_signed(Int, 64) -> (Int bsl 1) bxor (Int bsr (63))
.

% decode_signed(Int)
decode_signed(Int) -> case (Int band 1) of
	1 -> % odd (negative)
		-(Int bsr 1 + 1)
	; 0 ->
		Int bsr 1
	end
.

parse_varint(IntString) -> parse_varintI(IntString, [])
.

parse_varintI(<<Chunk:8/bits, IntString/binary>>, List) -> % Parse this chunk...
	<<Continues:1/bits, Value:7/bits>> = Chunk
	, NewList = [Value | List]
	, case Continues of
		<<1:1>> ->  % More to come...
		parse_varintI(IntString, NewList)
		;
		<<0:1>> -> % Last piece
		% Coalesce the 7-bit bitstrings, parse as integer, then output said int...
		Len = length(NewList) * 7,
		CoalescedBitstring = list_to_bitstring(NewList),
		<<MyInt:Len/integer>> = CoalescedBitstring,
		[MyInt, IntString]
	end
.


% parse_message(ByteString)
% Outputs [[{field_number, N}, {field_type, varint|string|int32|int64}, optional {value, Value}], ...]
parse_message(ByteString) -> parse_messageI(ByteString, [])
.
parse_messageI(<<>>, List) ->
	List
;
parse_messageI(<<KeyType:8/bits, ByteString/binary>>, List) ->
	parse_messageI([KeyType, <<ByteString/binary>>], List)
;
parse_messageI([<<FieldNumber:5/integer, FieldType:3/integer>> = _KeyType, <<ByteString/binary>>], List) ->
	[NewByteString, Data] = parse_message_type(FieldType, FieldNumber, ByteString)
	, parse_messageI(NewByteString, [Data | List])
.

parse_message_type(_FieldType = 0, FieldNumber, <<ByteString/binary>>) -> % varint
	[MyInt, NewByteString] = parse_varint(ByteString)
	, [NewByteString, [{field_number, FieldNumber}, {field_type, varint}, {value, MyInt}]]
;
parse_message_type(_FieldType = 1, FieldNumber, <<ByteString/binary>>) -> % int64
	<<MyInt:64/integer, NewByteString/binary>> = ByteString
	, [NewByteString, [{field_number, FieldNumber}, {field_type, int64}, {value, MyInt}]]
;
parse_message_type(_FieldType = 2, FieldNumber, <<ByteString/binary>>) -> % string
	[StringLength, NewByteString1] = parse_varint(ByteString)
	, <<String:StringLength/bytes, NewByteString2/bitstring>> = NewByteString1
	, [NewByteString2, [{field_number, FieldNumber}, {field_type, string}, {value, String}]]
;
parse_message_type(_FieldType = 5, FieldNumber, <<ByteString/binary>>) -> % int32
	<<MyInt:32/integer, NewByteString/binary>> = ByteString
	, [NewByteString, [{field_number, FieldNumber}, {field_type, int32}, {value, MyInt}]]
;
parse_message_type(FieldType, FieldNumber, <<ByteString/binary>>) ->
	[ByteString, [{field_number, FieldNumber}, {field_type, FieldType}]]
.