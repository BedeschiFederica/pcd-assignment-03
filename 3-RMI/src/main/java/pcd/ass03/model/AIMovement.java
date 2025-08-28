package pcd.ass03.model;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.Comparator;
import java.util.Optional;

/**
 * Module to manage the AI movement in a simple Agar.IO system.
 */
public class AIMovement implements Remote {

    private static Optional<Food> nearestFood(final Player player, final World world) throws RemoteException {
        return world.getFoods().stream()
                .min(Comparator.comparingDouble(f -> {
                    try {
                        return f.distanceTo(player);
                    } catch (RemoteException e) {
                        throw new RuntimeException(e);
                    }
                }));
    }

    public static void moveAI(final String playerName, final GameStateManager gameManager) throws RemoteException {
        final World world = gameManager.getWorld();
        final Optional<Player> aiOpt = world.getPlayerById(playerName);
        if (aiOpt.isPresent()) {
            final Player ai = aiOpt.get();
            final Optional<Food> foodOpt = nearestFood(ai, world);
            if (foodOpt.isPresent()) {
                Food food = foodOpt.get();
                final double dx = food.getX() - ai.getX();
                final double dy = food.getY() - ai.getY();
                final double distance = food.distanceTo(ai);
                if (distance > 0) {
                    gameManager.setPlayerDirection(playerName, dx / distance,  dy / distance);
                }
            } else {
                // No Food, Stop the player movement
                gameManager.setPlayerDirection(playerName, 0, 0);
            }
        }
    }
}
