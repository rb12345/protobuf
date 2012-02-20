ProtoBuf: Erlang parser for Protocol Buffers messages
=====================================================

The protobuf module is a simple library for parsing Protocol Buffers messages in the form of bitstrings.  It has the following API:

* `[[{field_number, FieldNumber}, {field_type, FieldType}, (optional) {value, Value}], ...] = parse_message(BitString)`
* `encode_signed(Int, 32)` - convert signed int32 to Procol Buffers wire format (ZigZag)
* `encode_signed(Int, 64)` - convert signed int64 to ZigZag format
* `decode_signed(Int)` - convert ZigZag number to signed integer

Example code:

    BitString = <<16#12, 16#07, 16#74, 16#65, 16#73, 16#74, 16#69, 16#6e, 16#67, 16#08, 16#96, 16#01>>.
    Message = protobuf:parse_message(BitString).
