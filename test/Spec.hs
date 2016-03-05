

main :: IO ()
main = putStrLn "Test suite not yet implemented"


---
-- test
-- test' = do
--   doc <- njeDoc $ NjeView "96144"
--   renderFile "out.html" doc


-- test'' :: IO ()
-- test'' = Resource.runResourceT $ do
--   let (api, query) = njeApiToQuery $ NjeFav 1
--   event <- njeStreamEvent api query
--   event $$+- sink
--   where 
--     sink = do
--       mval <- await
--       case mval of
--         Nothing  -> return ()
--         Just val -> do
--           Trans.liftIO $ go val
--           sink
--     go (XMLTypes.EventBeginElement e _) = print e
--     go _ = return ()
