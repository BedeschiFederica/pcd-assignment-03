package pcd.ass03;

import pcd.ass03.model.*;

import java.rmi.registry.LocateRegistry;
import java.rmi.server.UnicastRemoteObject;
import java.util.List;

public class GameManager {

    private static final int WORLD_WIDTH = 1000;
    private static final int WORLD_HEIGHT = 1000;
    private static final int NUM_PLAYERS = 4; // p1, p2, p3, p4
    private static final int NUM_FOODS = 100;
    private static final long GAME_TICK_MS = 30; // Corresponds to ~33 FPS

    public static void main(String[] args) {

        try {
            final List<Player> initialPlayers = GameInitializer.initialPlayers(NUM_PLAYERS, WORLD_WIDTH, WORLD_HEIGHT);
            final List<Food> initialFoods = GameInitializer.initialFoods(NUM_FOODS, WORLD_WIDTH, WORLD_HEIGHT);
            final World initialWorld = new World(WORLD_WIDTH, WORLD_HEIGHT, initialPlayers, initialFoods);
            final GameStateManager gameManager = new DefaultGameStateManager(initialWorld);
            /*var count = new CounterImpl(0);
            var countStub = (Counter) UnicastRemoteObject.exportObject(count, 0);

            var registry = LocateRegistry.getRegistry();
            registry.rebind("countObj", countStub);

            log("Count object registered.");*/


        } catch (Exception e) {
            log("Server exception: " + e.toString());
            e.printStackTrace();
        }
    }

    private static void log(String msg) {
        System.out.println("[ " + System.currentTimeMillis() + " ][ Main ] " + msg);
    }
}
