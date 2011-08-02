#define LOGLEVEL DEBUG
#define LOG_NAME "server"
#define LOG(p) liftIO . Log.logM LOG_NAME Log.p $ \
  (( __BASE_FILE__ ++ ":" ++ show ( __LINE__ :: Int ) ++ ":") ++)

#define _UNDEF error ( "UNDEFINED AT: " ++ __FILE__ ++ ":" ++ show (__LINE__ :: Int) )
#define _ERROR(msg) error ( "FATAL ERROR: " ++ msg ++ "AT: " ++ __FILE__ ++ ":" ++ show (__LINE__ :: Int) )
#define _THROW(e) Control.throwIO $ e $ "AT: " ++ __FILE__ ++ ":" ++ show ( __LINE__ :: Int)
#define _THROWS(e,s) (( LOG(ERROR) (show (e (s)) ) ) >> (Control.throwIO $ e $ (s) ++ " AT: " ++ __FILE__ ++":" ++ show ( __LINE__ :: Int) ))

#if GHC7
#define HAMLET hamlet
#else
#define HAMLET $hamlet
#endif

