package pcd.ass03.model;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Entity extends Remote {
    String getId() throws RemoteException;
    double getMass() throws RemoteException;
    double getX() throws RemoteException;
    double getY() throws RemoteException;
    double getRadius() throws RemoteException;

    default double distanceTo(final Entity other) throws RemoteException {
        double dx = getX() - other.getX();
        double dy = getY() - other.getY();
        return Math.hypot(dx, dy);
    }
}
