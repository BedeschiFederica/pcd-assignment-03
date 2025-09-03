package pcd.ass03.model;

import java.rmi.RemoteException;
import java.util.Optional;

public class WinnerListenerImpl implements WinnerListener {

    private Optional<String> winnerId = Optional.empty();

    @Override
    public Optional<String> getWinnerId() throws RemoteException {
        return this.winnerId;
    }

    @Override
    public synchronized void setWinner(String playerId) throws RemoteException {
        this.winnerId = Optional.of(playerId);
    }
}
