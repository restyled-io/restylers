  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

ormolu

  $ run_restyler ormolu signature.hs
  -foo
  - :: MonadIO m
  - -> Text -> Text
  - -> SqlPersistT m ()
  - foo = undefined
  +foo ::
  +  MonadIO m ->
  +  Text ->
  +  Text ->
  +  SqlPersistT m ()
  +    foo = undefined
