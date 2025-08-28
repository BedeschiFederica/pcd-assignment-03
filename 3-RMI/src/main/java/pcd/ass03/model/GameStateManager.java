package pcd.ass03.model;

import java.io.Serializable;
import java.rmi.Remote;
import java.rmi.RemoteException;

public interface GameStateManager extends Serializable, Remote {
    World getWorld() throws RemoteException;
    void setPlayerDirection(final String playerId, final double dx, final double dy) throws RemoteException;
    void tick() throws RemoteException;
    void addPlayer(Player player) throws RemoteException;
}
