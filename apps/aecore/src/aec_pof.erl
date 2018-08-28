%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Proof of Fraud transaction
%%% @end
%%%=============================================================================

-module(aec_pof).

-include("blocks.hrl").

%% Behavior API
-export([new/2,
         serialization_template/1,
         serialize/1,
         deserialize/2
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

serialize(#{header := Header,
           fraud_header := FraudHeader}) ->
    {version(),
     [ {header, Header}
     , {fraud_header, FraudHeader}]}.

deserialize(?POF_VSN,
            [ {header, Header}
            , {fraud_header, FraudHeader}]) ->
    #{header => Header,
      fraud_header => FraudHeader}.

serialization_template(?POF_VSN) ->
    [ {header, binary}
    , {fraud_header, binary}].

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

version() ->
    ?POF_VSN.
