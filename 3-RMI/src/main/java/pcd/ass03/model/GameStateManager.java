package pcd.ass03.model;

import java.io.Serializable;
import java.rmi.Remote;
import java.rmi.RemoteException;

public interface GameStateManager extends Serializable, Remote {
    World getWorld() throws RemoteException;
    void setPlayerDirection(String playerId, double dx, double dy) throws RemoteException;
    void tick() throws RemoteException;
    String addPlayer() throws RemoteException;
    void removePlayer(String playerId) throws RemoteException;
}
