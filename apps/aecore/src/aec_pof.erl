%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Proof of Fraud transaction
%%% @end
%%%=============================================================================

-module(aec_pof).

-include("blocks.hrl").

%% Behavior API
-export([deserialize_from_binary/1,
         new/2,
         serialization_template/1,
         serialize_to_binary/1
        ]).

%% Getters
-export([header/1,
         fraud_header/1]).

%% Validators
-export([check_fraud_headers/2]).


-define(POF_VSN, 1).

-type pof() :: 'no_fraud' | map().
-export_type([pof/0]).

new(Header, FraudHeader) ->
    #{header => Header, fraud_header => FraudHeader}.

-spec serialize_to_binary(map()) -> binary().
serialize_to_binary(#{header       := Header,
                      fraud_header := FraudHeader}) ->
    SerializedHdr      = aec_headers:serialize_to_binary(Header),
    SerializedFraudHdr = aec_headers:serialize_to_binary(FraudHeader),
    aec_object_serialization:serialize(
      pof,
      ?POF_VSN,
      serialization_template(?POF_VSN),
      [{header, SerializedHdr}, {fraud_header, SerializedFraudHdr}]).

-spec deserialize_from_binary(binary()) -> {'ok', map()} | {'error', term()}.
deserialize_from_binary(PoFBin) when is_binary(PoFBin) ->
    [ {header, SerializedHdr}
    , {fraud_header, SerializedFraudHdr}
    ] = aec_object_serialization:deserialize(
          pof,
          ?POF_VSN,
          serialization_template(?POF_VSN),
          PoFBin),
    #{header       => aec_headers:deserialize_from_binary(SerializedHdr),
      fraud_header => aec_headers:deserialize_from_binary(SerializedFraudHdr)}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec header(map()) -> aec_id:header().
header(#{header := Header}) ->
    Header.

-spec fraud_header(map()) -> aec_id:header().
fraud_header(#{fraud_header := FraudHeader}) ->
    FraudHeader.

%%%===================================================================
%%% Validation
%%%===================================================================

-spec check_fraud_headers(map(), aec_trees:trees()) ->
                                 ok | {error, term()}.
check_fraud_headers(#{header := _Header, fraud_header := _FraudHeader} = _Tx,
                    _Trees) ->
    %% TODO:
    %% 1. deserialize header
    %% 2. deserialize second header
    %% 3. cross-check offender's  pull pub key
    %% 4. get offender's key block
    %% 5. check signatures
    %% 6. check prev
    %% 7. check height - we can only punish before coinbase kicks in
    ok.

%%%===================================================================
%%% Internals
%%%===================================================================

serialization_template(?POF_VSN) ->
    [ {header, binary}
    , {fraud_header, binary}].
