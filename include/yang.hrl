%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    YANG definitions
%%% @end
%%% Created :  13 Jan 2012 by generage_yang_hrl
-ifndef(__YANG_HRL__).
-define(__YANG_HRL__, true).
%%
%% anyxml
%%
-record('yang-anyxml',{
  arg=[],
  'when' = undefined,
  'if-feature' = [],
  must = [],
  config = undefined,
  mandatory = undefined,
  status = undefined,
  description = undefined,
  reference = undefined
 }).

%%
%% argument
%%
-record('yang-argument',{
  arg=[],
  'yin-element' = undefined
 }).

%%
%% augment
%%
-record('yang-augment',{
  arg=[],
  'when' = undefined,
  'if-feature' = [],
  status = undefined,
  description = undefined,
  reference = undefined,
  data = []
 }).

%%
%% base
%%
-record('yang-base',{ arg=[] }).

%%
%% belongs-to
%%
-record('yang-belongs-to',{
  arg=[],
  prefix = undefined
 }).

%%
%% bit
%%
-record('yang-bit',{
  arg=[],
  position = undefined,
  status = undefined,
  description = undefined,
  reference = undefined
 }).

%%
%% case
%%
-record('yang-case',{
  arg=[],
  'when' = undefined,
  'if-feature' = [],
  status = undefined,
  description = undefined,
  reference = undefined,
  data = []
 }).

%%
%% choice
%%
-record('yang-choice',{
  arg=[],
  'when' = undefined,
  'if-feature' = [],
  default = undefined,
  config = undefined,
  mandatory = undefined,
  status = undefined,
  description = undefined,
  reference = undefined,
  data = []
 }).

%%
%% config
%%
-record('yang-config',{ arg=[] }).

%%
%% contact
%%
-record('yang-contact',{ arg=[] }).

%%
%% container
%%
-record('yang-container',{
  arg=[],
  'when' = undefined,
  'if-feature' = [],
  must = [],
  presence = undefined,
  config = undefined,
  status = undefined,
  description = undefined,
  reference = undefined,
  definitions = [],
  data = []
 }).

%%
%% default
%%
-record('yang-default',{ arg=[] }).

%%
%% description
%%
-record('yang-description',{ arg=[] }).

%%
%% enum
%%
-record('yang-enum',{
  arg=[],
  value = undefined,
  status = undefined,
  description = undefined,
  reference = undefined
 }).

%%
%% error-app-tag
%%
-record('yang-error-app-tag',{ arg=[] }).

%%
%% error-message
%%
-record('yang-error-message',{ arg=[] }).

%%
%% extension
%%
-record('yang-extension',{
  arg=[],
  argument = undefined,
  status = undefined,
  description = undefined,
  reference = undefined
 }).

%%
%% deviation
%%
-record('yang-deviation',{
  arg=[],
  description = undefined,
  reference = undefined,
  data = undefined
 }).

%%
%% deviate+not-supported
%%
-record('yang-deviate+not-supported',{ arg="not-supported" }).

%%
%% deviate+add
%%
-record('yang-deviate+add',{
  arg="add",
  units = undefined,
  must = [],
  unique = [],
  default = undefined,
  config = undefined,
  mandatory = undefined,
  'min-elements' = undefined,
  'max-elements' = undefined
 }).

%%
%% deviate+delete
%%
-record('yang-deviate+delete',{
  arg="delete",
  units = undefined,
  must = [],
  unique = [],
  default = undefined
 }).

%%
%% deviate+replace
%%
-record('yang-deviate+replace',{
  arg="replace",
  type = undefined,
  units = undefined,
  default = undefined,
  config = undefined,
  mandatory = undefined,
  'min-elements' = undefined,
  'max-elements' = undefined
 }).

%%
%% feature
%%
-record('yang-feature',{
  arg=[],
  'if-feature' = [],
  status = undefined,
  description = undefined,
  reference = undefined
 }).

%%
%% fraction-digits
%%
-record('yang-fraction-digits',{ arg=[] }).

%%
%% grouping
%%
-record('yang-grouping',{
  arg=[],
  status = undefined,
  description = undefined,
  reference = undefined,
  definitions = [],
  data = []
 }).

%%
%% identity
%%
-record('yang-identity',{
  arg=[],
  base = undefined,
  status = undefined,
  description = undefined,
  reference = undefined
 }).

%%
%% if-feature
%%
-record('yang-if-feature',{ arg=[] }).

%%
%% import
%%
-record('yang-import',{
  arg=[],
  prefix = undefined,
  'revision-date' = undefined
 }).

%%
%% include
%%
-record('yang-include',{
  arg=[],
  'revision-date' = undefined
 }).

%%
%% input
%%
-record('yang-input',{
  arg=[],
  definitions = [],
  data = []
 }).

%%
%% key
%%
-record('yang-key',{ arg=[] }).

%%
%% leaf
%%
-record('yang-leaf',{
  arg=[],
  'when' = undefined,
  'if-feature' = [],
  type = undefined,
  units = undefined,
  must = [],
  default = undefined,
  config = undefined,
  mandatory = undefined,
  status = undefined,
  description = undefined,
  reference = undefined
 }).

%%
%% leaf-list
%%
-record('yang-leaf-list',{
  arg=[],
  'when' = undefined,
  'if-feature' = [],
  type = undefined,
  units = undefined,
  must = [],
  config = undefined,
  'min-elements' = undefined,
  'max-elements' = undefined,
  'ordered-by' = undefined,
  status = undefined,
  description = undefined,
  reference = undefined
 }).

%%
%% length
%%
-record('yang-length',{
  arg=[],
  'error-message' = undefined,
  'error-app-tag' = undefined,
  description = undefined,
  reference = undefined
 }).

%%
%% list
%%
-record('yang-list',{
  arg=[],
  'when' = undefined,
  'if-feature' = [],
  must = [],
  key = undefined,
  unique = [],
  config = undefined,
  'min-elements' = undefined,
  'max-elements' = undefined,
  'ordered-by' = undefined,
  status = undefined,
  description = undefined,
  reference = undefined,
  definitions = [],
  data = []
 }).

%%
%% mandatory
%%
-record('yang-mandatory',{ arg=[] }).

%%
%% max-elements
%%
-record('yang-max-elements',{ arg=[] }).

%%
%% min-elements
%%
-record('yang-min-elements',{ arg=[] }).

%%
%% module
%%
-record('yang-module',{
  arg=[],
  'yang-version' = undefined,
  namespace = undefined,
  prefix = undefined,
  import = undefined,
  include = undefined,
  organization = undefined,
  contact = undefined,
  description = undefined,
  reference = undefined,
  revision = [],
  body = []
 }).

%%
%% must
%%
-record('yang-must',{
  arg=[],
  'error-message' = undefined,
  'error-app-tag' = undefined,
  description = undefined,
  reference = undefined
 }).

%%
%% namespace
%%
-record('yang-namespace',{ arg=[] }).

%%
%% notification
%%
-record('yang-notification',{
  arg=[],
  'if-feature' = [],
  status = undefined,
  description = undefined,
  reference = undefined,
  definitions = [],
  data = []
 }).

%%
%% ordered-by
%%
-record('yang-ordered-by',{ arg=[] }).

%%
%% organization
%%
-record('yang-organization',{ arg=[] }).

%%
%% output
%%
-record('yang-output',{
  arg=[],
  definitions = [],
  data = []
 }).

%%
%% path
%%
-record('yang-path',{ arg=[] }).

%%
%% pattern
%%
-record('yang-pattern',{
  arg=[],
  'error-message' = undefined,
  'error-app-tag' = undefined,
  description = undefined,
  reference = undefined
 }).

%%
%% position
%%
-record('yang-position',{ arg=[] }).

%%
%% prefix
%%
-record('yang-prefix',{ arg=[] }).

%%
%% presence
%%
-record('yang-presence',{ arg=[] }).

%%
%% range
%%
-record('yang-range',{
  arg=[],
  'error-message' = undefined,
  'error-app-tag' = undefined,
  description = undefined,
  reference = undefined
 }).

%%
%% reference
%%
-record('yang-reference',{ arg=[] }).

%%
%% refine
%%
-record('yang-refine',{
  arg=[],
  must = [],
  default = undefined,
  presence = undefined,
  config = undefined,
  'min-elements' = undefined,
  'max-elements' = undefined,
  mandatory = undefined,
  description = undefined,
  reference = undefined
 }).

%%
%% require-instance
%%
-record('yang-require-instance',{ arg=[] }).

%%
%% revision
%%
-record('yang-revision',{
  arg=[],
  description = undefined,
  reference = undefined
 }).

%%
%% revision-date
%%
-record('yang-revision-date',{ arg=[] }).

%%
%% rpc
%%
-record('yang-rpc',{
  arg=[],
  'if-feature' = [],
  status = undefined,
  description = undefined,
  reference = undefined,
  definitions = [],
  input = undefined,
  output = undefined
 }).

%%
%% status
%%
-record('yang-status',{ arg=[] }).

%%
%% submodule
%%
-record('yang-submodule',{
  arg=[],
  'yang-version' = undefined,
  'belongs-to' = undefined,
  import = undefined,
  include = undefined,
  organization = undefined,
  contact = undefined,
  description = undefined,
  reference = undefined,
  revision = [],
  body = []
 }).

%%
%% type
%%
-record('yang-type',{
  arg=[],
  base = undefined,
  bit = undefined,
  enum = undefined,
  'fraction-digits' = undefined,
  length = undefined,
  path = undefined,
  pattern = [],
  range = undefined,
  'require-instance' = undefined,
  type = undefined
 }).

%%
%% typedef
%%
-record('yang-typedef',{
  arg=[],
  type = undefined,
  units = undefined,
  default = undefined,
  status = undefined,
  description = undefined,
  reference = undefined
 }).

%%
%% unique
%%
-record('yang-unique',{ arg=[] }).

%%
%% units
%%
-record('yang-units',{ arg=[] }).

%%
%% uses
%%
-record('yang-uses',{
  arg=[],
  'when' = undefined,
  'if-feature' = [],
  status = undefined,
  description = undefined,
  reference = undefined,
  refine = [],
  augment@uses = []
 }).

%%
%% value
%%
-record('yang-value',{ arg=[] }).

%%
%% when
%%
-record('yang-when',{
  arg=[],
  description = undefined,
  reference = undefined
 }).

%%
%% yang-version
%%
-record('yang-yang-version',{ arg="1" }).

%%
%% yin-element
%%
-record('yang-yin-element',{ arg=[] }).

-endif.
