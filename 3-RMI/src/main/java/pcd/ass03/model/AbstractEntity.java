package pcd.ass03.model;


import java.rmi.RemoteException;

public abstract class AbstractEntity implements Entity {
    private final String id;
    private final double x;
    private final double y;
    private final double mass;
    private final double radius;

    protected AbstractEntity(final String id, final double x, final double y, final double mass) {
        this.id = id;
        this.x = x;
        this.y = y;
        this.mass = mass;
        this.radius = Math.sqrt(mass / Math.PI);
    }

    @Override
    public String getId() throws RemoteException {
        return id;
    }

    @Override
    public double getX() throws RemoteException {
        return x;
    }

    @Override
    public double getY() throws RemoteException {
        return y;
    }

    @Override
    public double getMass() throws RemoteException {
        return mass;
    }

    @Override
    public double getRadius() throws RemoteException {
        return radius;
    }
}