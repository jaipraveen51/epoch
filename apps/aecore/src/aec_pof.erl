%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Proof of Fraud transaction
%%% @end
%%%=============================================================================

-module(aec_pof).

-include("blocks.hrl").

%% Behavior API
-export([new/1,
         check/5,
         serialization_template/1,
         serialize/1,
         deserialize/2
        ]).

%% Getters
-export([header/1,
         fraud_header/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(pof, {
          header       :: aec_id:headers(),
          fraud_header :: aec_id:headers()}).

-define(POF_VSN, 1).

-opaque pof() :: #pof{}.
-export_type([pof/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, pof()}.
new(#{header       := Header,
      fraud_header := FraudHeader}) when is_binary(Header),
                                         is_binary(FraudHeader) ->
    PoF = #pof{header      = Header,
               fraud_header = FraudHeader},
    {ok, PoF}.

-spec check(pof(), aetx:tx_context(), aec_trees:trees(), aec_blocks:height(), non_neg_integer()) ->
                   {ok, aec_trees:trees()} | {error, term()}.
check(#pof{} = PoFTx, _Context, Trees, Height, _ConsensusVersion) ->
    Checks = [fun() -> check_height(PoFTx, Height) end,
              fun() -> check_fraud_headers(PoFTx, Trees) end],

    case aeu_validation:run(Checks) of
        ok ->
            ok;
        {error, _Reason} = Error ->
            Error
    end.


serialize(#pof{
             header = Header,
             fraud_header = FraudHeader}) ->
    {version(),
     [ {header, Header}
     , {fraud_header, FraudHeader}]}.

deserialize(?POF_VSN,
            [ {header, Header}
            , {fraud_header, FraudHeader}]) ->
    #pof{
       header = Header,
       fraud_header = FraudHeader}.

serialization_template(?POF_VSN) ->
    [ {header, binary}
    , {fraud_header, binary}].

%%%===================================================================
%%% Getters
%%%===================================================================

-spec header(pof()) -> aec_id:header().
header(#pof{header = Header}) ->
    Header.

-spec fraud_header(pof()) -> aec_id:header().
fraud_header(#pof{fraud_header = FraudHeader}) ->
    FraudHeader.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_height(_PoF, _Height) ->
    %% TODO: deserialize and compare heights (must by off by one)
    ok.

-spec check_fraud_headers(pof(), aec_trees:trees()) ->
                                 ok | {error, term()}.
check_fraud_headers(#pof{header = _Header, fraud_header = _FraudHeader} = _Tx,
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
