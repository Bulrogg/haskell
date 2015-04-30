{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{- Ce programme montre comment créer un réseau d'Acteurs en Haskell (librairie Cloud Haskell)
 - Après l'initialisation, chaque Acteur connait la liste et l'adresse (ProcessId) des autres Acteurs
 - Pour enrichir la communication entre Acteurs, enrichir la liste des messages possibles dans la liste de receiveWait
 - Un acteur a comme type "Process ()"
 - Chaque acteur s'identifie avec un Int (0->3)
 - Normalement, tous les messages échangés doivent implémenter Binary et Typeable. Les messages de ce tuto le font déjà par défaut (Int, ...)
 -}

module Main where

--import Data.Binary
--import Data.Typeable
import qualified Data.Map as M
import Control.Monad (forM_, forever)
import Control.Concurrent (threadDelay)

-- Les librairies liées à Cloud Haskell
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Serializable
import Network.Transport.TCP -- On travaille sur un réseau local TCP, donc on choisit ce backend de Cloud Haskell
import Control.Distributed.Process.Node (runProcess)


-- Cette fonction définir le processus Peer (un acteur)
-- Le peer envoit au maitre son adresse (selfPID) et reçoit les adresses des autres acteurs (allPeers)
peer :: ProcessId -> Int -> Process ()
peer masterPID i = do
    liftIO $ putStrLn $ "I am peer n°" ++ (show i)
    selfPID <- getSelfPid
    send masterPID (selfPID,i) -- j'envoie mon adresse au master
    loop M.empty
  where
    loop peers = do
      say $ "Peer waiting for next message (" ++ (show $ M.size peers) ++ " peers known)"
      receiveWait [
          match (\(allPeers :: [(Int,ProcessId)]) -> loop (M.fromList allPeers)) -- Recevoir la liste des Peers
          ]
      loop peers
    
-- On définit le boilerplate qui permet d'appeler peer sur une autre machine (oui c'est de la magie)
-- $(remotable ['peer])
-- on fait entrer peer dans myRemoteTable, la liste des fonctions appelables à distance
myRemoteTable :: RemoteTable
myRemoteTable = initRemoteTable
--myRemoteTable = Main.__remoteTable initRemoteTable


-- Broadcaster un message à tous les Acteurs de la liste
broadcast :: Serializable a => [ProcessId] -> a -> Process ()
broadcast pids m = mapM_ (flip send m) pids

-- Cette fonction définit le processus Master (un acteur)
-- NB: Il n'existe pas forcement de master dans le modèle à Acteurs.
--     Ce master sert à fournir aux peers (les instances utiles) leurs adresses respectives
-- La fonction receiveWait bloque jusqu'à la réception d'un message, et dispatch en fonction de son type
-- Le paramètre peers est la collection des adresses des peers. Elle est mise à jour au fil du temps.
master peers = forever $ do
  receiveWait [
     -- Quand un Peer se déclare, le présenter aux autres
     match (\(sender :: ProcessId, i :: Int) -> do
        broadcast (sender:M.elems peers) (M.toList peers) -- j'envoie les adresses de tout le monde à tout le monde
        master (M.insert i sender peers))                 -- j'enregistre ce nouveau peer en local
   -- Autres exemples de messages, dont trois notifications (exceptions distantes)
   , match (\((sender :: ProcessId), (msg :: String)) -> send sender msg >> return ())
   , match (\(m :: String) -> say $ "printing " ++ m)
   , match (\(ProcessMonitorNotification _ pid reason) -> say "ProcessMonitorNotification!")
   , match (\(NodeMonitorNotification  _ pid reason) -> say "NodeMonitorNotification!")
   , match (\(PortMonitorNotification   _ pid reason) -> say "PortMonitorNotification!")
   ]
    
-- Démarrer (sur ce noeud) le master (rôle de coordinateur) et n Acteurs
-- Dans une prochaine version, il faudra les démarrer sur d'autres machines
ignition :: Int -> Process ()
ignition n = do
    masterPID <- spawnLocal (master M.empty)
    forM_ [0..(n-1)] (\i -> spawnLocal $ peer masterPID i)

main :: IO ()
main = do
  Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node <- newLocalNode t myRemoteTable
  let n = 4
  runProcess node (ignition 4)
  liftIO $ threadDelay (10*200000) -- wait a while
  print "End"

{- Usage et résultat attendu:
 - $ ghc --make ActorsTutorial.hs -o server
 - $ ./server 
 - I am peer n°0
 - I am peer n°1
 - I am peer n°2
 - I am peer n°3
 - Thu Feb 21 14:48:50 UTC 2013 pid://127.0.0.1:10501:0:5: Peer waiting for next message (0 peers known)
 - Thu Feb 21 14:48:50 UTC 2013 pid://127.0.0.1:10501:0:6: Peer waiting for next message (0 peers known)
 - Thu Feb 21 14:48:50 UTC 2013 pid://127.0.0.1:10501:0:7: Peer waiting for next message (0 peers known)
 - Thu Feb 21 14:48:50 UTC 2013 pid://127.0.0.1:10501:0:8: Peer waiting for next message (0 peers known)
 - Thu Feb 21 14:48:50 UTC 2013 pid://127.0.0.1:10501:0:5: Peer waiting for next message (0 peers known)
 - Thu Feb 21 14:48:50 UTC 2013 pid://127.0.0.1:10501:0:5: Peer waiting for next message (1 peers known)
 - Thu Feb 21 14:48:50 UTC 2013 pid://127.0.0.1:10501:0:6: Peer waiting for next message (1 peers known)
 - Thu Feb 21 14:48:50 UTC 2013 pid://127.0.0.1:10501:0:7: Peer waiting for next message (2 peers known)
 - Thu Feb 21 14:48:50 UTC 2013 pid://127.0.0.1:10501:0:5: Peer waiting for next message (2 peers known)
 - Thu Feb 21 14:48:50 UTC 2013 pid://127.0.0.1:10501:0:6: Peer waiting for next message (2 peers known)
 - Thu Feb 21 14:48:50 UTC 2013 pid://127.0.0.1:10501:0:8: Peer waiting for next message (3 peers known)
 - Thu Feb 21 14:48:50 UTC 2013 pid://127.0.0.1:10501:0:5: Peer waiting for next message (3 peers known)
 - Thu Feb 21 14:48:50 UTC 2013 pid://127.0.0.1:10501:0:6: Peer waiting for next message (3 peers known)
 - Thu Feb 21 14:48:50 UTC 2013 pid://127.0.0.1:10501:0:7: Peer waiting for next message (3 peers known)
 - "End"
 -}
