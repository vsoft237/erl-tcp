-ifndef(PACKET_HRL).
-define(PACKET_HRL, true).

-define(PACK_HEAD_LENGTH, 11).
-define(UNPACK_HEAD_LENGTH, 9).
-define(NO_ERR_ID, 0).
-define(EMPTY_DATA_SIZE, 0).

%% DataStatus
-define(RAW_DATA, 0).
-define(COMPRESSION_DATA, 1).
-define(ENCRYPTION_DATA, 2).

-endif.
