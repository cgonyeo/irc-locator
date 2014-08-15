import Network.Socket
import System.IO
import Control.Concurrent
import Network.Connection
import qualified Data.ByteString.Char8 as BSC
import Aws
import Aws.Core
import Aws.General
import Aws.Sns
import Data.IORef
import Data.Time.Clock
import qualified Data.Text as DT
 
main :: IO ()
main = do
    -- create socket
    sock <- socket AF_INET Stream 0
    -- make socket immediately reusable - eases debugging.
    setSocketOption sock ReuseAddr 1
    -- listen on TCP port 4242
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    -- allow a maximum of 2 outstanding connections
    listen sock 2
    sendToSns "test"
    mainLoop sock
 
mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock
    forkIO (runConn conn)
    mainLoop sock
 
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    con <- startTls
    forkIO (serverToClient con hdl)
    forkIO (clientToServer hdl con)
    return ()
    --hClose hdl
    --connectionClose con

serverToClient :: Connection -> Handle -> IO ()
serverToClient con hdl = do
    msg <-  connectionGet con 1024
    putStrLn $ "Server says: " ++ (BSC.unpack msg)
    hPutStrLn hdl $ BSC.unpack msg
    serverToClient con hdl

clientToServer :: Handle -> Connection -> IO ()
clientToServer hdl con = do
    msg <- hGetLine hdl
    putStrLn $ "Client says: " ++ msg
    connectionPut con $ BSC.pack msg
    clientToServer hdl con

startTls :: IO Connection
startTls = do
    ctx <- initConnectionContext
    con <- connectTo ctx $ ConnectionParams
                            { connectionHostname  = "skynet.csh.rit.edu"
                            , connectionPort      = 6697
                            , connectionUseSecure = Just $ TLSSettingsSimple True False False
                            , connectionUseSocks  = Nothing
                            }
    return con

sendToSns :: String -> IO ()
sendToSns msg = do
    snsioref <- newIORef []
    creds <- makeCredentials (BSC.pack "asdf") (BSC.pack "asdf")
    let cfg = Aws.Configuration (ExpiresIn 3600) creds (Aws.defaultLog Aws.Debug)
    let snsCfg = SnsConfiguration HTTPS UsWest2
    let arn = Arn ServiceNamespaceSns (Just UsWest2) (Just $ AccountId $ DT.pack "1337") ([DT.pack "asdf"])
    topics <- simpleAws cfg snsCfg $ Publish (snsMessage (DT.pack msg)) Nothing Nothing (Right arn)
    putStrLn $ show topics
