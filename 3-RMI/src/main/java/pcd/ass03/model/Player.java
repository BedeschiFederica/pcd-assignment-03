package pcd.ass03.model;

import java.rmi.RemoteException;

public class Player extends AbstractEntity {
    public Player(final String id, final double x, final double y, final double mass) {
        super(id, x, y, mass);
    }


    public Player grow(Entity entity) throws RemoteException {
        return new Player(getId(), getX(), getY(), getMass() + entity.getMass());
    }

    public Player moveTo(double newX, double newY) throws RemoteException {
        return new Player(getId(), newX, newY, getMass());
    }
}
