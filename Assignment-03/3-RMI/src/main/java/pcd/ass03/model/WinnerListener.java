package pcd.ass03.model;

import java.io.Serializable;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.Optional;

public interface WinnerListener extends Remote, Serializable {

    Optional<String> getWinnerId() throws RemoteException;

    void setWinner(String playerId) throws RemoteException;

}
