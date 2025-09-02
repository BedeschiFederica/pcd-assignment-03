package pcd.ass03;

import pcd.ass03.model.*;
import pcd.ass03.view.GlobalView;

import javax.swing.*;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.server.UnicastRemoteObject;
import java.util.*;
import java.util.Timer;

public class GameManager {

    private static final String MANAGER_NAME = "manager";
    private static final int WORLD_WIDTH = 1000;
    private static final int WORLD_HEIGHT = 1000;
    private static final int NUM_FOODS = 100;
    private static final long GAME_TICK_MS = 30; // Corresponds to ~33 FPS
    private static final int PORT = 0;

    public static void main(String[] args) {

        try {
            final List<Food> initialFoods = GameInitializer.initialFoods(NUM_FOODS, WORLD_WIDTH, WORLD_HEIGHT);
            final World initialWorld = new World(WORLD_WIDTH, WORLD_HEIGHT, new ArrayList<>(), initialFoods);
            final GameStateManager gameManager = new DefaultGameStateManager(initialWorld);
            final var managerStub = (GameStateManager) UnicastRemoteObject.exportObject(gameManager, PORT);

            final GlobalView globalView = new GlobalView(managerStub);
            SwingUtilities.invokeLater(() -> globalView.setVisible(true));

            var registry = LocateRegistry.getRegistry();
            registry.rebind(MANAGER_NAME, managerStub);

            final Timer timer = new Timer(true); // Use daemon thread for timer
            timer.scheduleAtFixedRate(new TimerTask() {
                @Override
                public void run() {
                    try {
                        gameManager.tick();
                    } catch (final RemoteException e) {
                        log("Server exception in tick: " + e);
                        e.printStackTrace();
                    }
                    SwingUtilities.invokeLater(globalView::repaintView);
                }
            }, 0, GAME_TICK_MS);
        } catch (final RemoteException e) {
            log("Server exception: " + e);
            e.printStackTrace();
        }
    }

    private static void log(String msg) {
        System.out.println("[ " + System.currentTimeMillis() + " ][ Main ] " + msg);
    }
}
