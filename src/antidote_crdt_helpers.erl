%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 SyncFree Consortium.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(antidote_crdt_helpers).
%%
%% This module includes some useful function, which can be shared by multiple implementations


-export([orddict_merge/3, select_keys/3]).


%% merges two orddicts using a function for merging single entries
%% unlike orddict:merge, the function is also called when an entry only exists in one of the dicts
%% the function also allows to return 'none', which will remove the entry from the result
-spec orddict_merge(orddict:orddict(K, V1), orddict:orddict(K, V2), fun((K, none|V1, none|V2) -> none | V3)) -> orddict:orddict(K, V3).
orddict_merge([], [], M) when is_function(M, 3) ->
  [];
orddict_merge([], [{K, V} | Ys], M) ->
  case M(K, none, V) of
    none -> orddict_merge([], Ys, M);
    X -> [{K, X} | orddict_merge([], Ys, M)]
  end;
orddict_merge([{K, V} | Xs], [], M) ->
  case M(K, V, none) of
    none -> orddict_merge(Xs, [], M);
    X -> [{K, X} | orddict_merge(Xs, [], M)]
  end;
orddict_merge([{KX, VX} | Xs], [{KY, VY} | Ys], M) ->
  if
    KX < KY ->
      case M(KX, VX, none) of
        none ->
          orddict_merge(Xs, [{KY, VY} | Ys], M);
        V2 ->
          [{KX, V2} | orddict_merge(Xs, [{KY, VY} | Ys], M)]
      end;
    KY < KX ->
      case M(KY, none, VY) of
        none ->
          orddict_merge([{KX, VX} | Xs], Ys, M);
        V2 ->
          [{KY, V2} | orddict_merge([{KX, VX} | Xs], Ys, M)]
      end;
    true ->  % KX == KY
      case M(KX, VX, VY) of
        none ->
          orddict_merge(Xs, Ys, M);
        V2 ->
          [{KX, V2} | orddict_merge(Xs, Ys, M)]
      end
  end.

% select keys from an orddict given an ordset of keys
% if a searched key is not present, use the given default value
-spec select_keys(Keys :: ordsets:ordset(K), Dict :: orddict:orddict(K, V), Default) -> orddict:orddict(K, V|Default).
select_keys([], _, _) ->
  [];
select_keys(Ks, [], Default) ->
  [{K, Default} || K <- Ks];
select_keys([K | Ks], [{KX, VX} | Xs], Default) ->
  if
    K < KX ->
      [{K, Default} | select_keys(Ks, [{KX, VX} | Xs], Default)];
    KX < K ->
      select_keys([K | Ks], Xs, Default);
    true ->  % K == KX
      [{K, VX} | select_keys(Ks, Xs, Default)]
  end.
