package pcd.ass03.model;

import java.rmi.Remote;
import java.rmi.RemoteException;

public class EatingManager implements Remote {

    private static final double MASS_MARGIN = 1.1; // 10% bigger to eat

    private static boolean collides(final Entity e1, final Entity e2) throws RemoteException {
        return e1.distanceTo(e2) < (e1.getRadius() + e2.getRadius());
    }

    public static boolean canEatFood(final Player player, final Food food) throws RemoteException {
        return collides(player, food) && player.getMass() > food.getMass();
    }

    public static boolean canEatPlayer(final Player player, final Player other) throws RemoteException {
        return collides(player, other) && player.getMass() > other.getMass() * MASS_MARGIN;
    }
}
