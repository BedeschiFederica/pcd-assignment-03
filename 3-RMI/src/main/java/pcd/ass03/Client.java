package pcd.ass03;

import pcd.ass03.model.GameStateManager;
import pcd.ass03.model.Player;
import pcd.ass03.view.LocalView;

import javax.swing.*;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.Random;
import java.util.Timer;
import java.util.TimerTask;

public class Client {

	private static final String MANAGER_NAME = "manager";
    private static final long GAME_TICK_MS = 30; // Corresponds to ~33 FPS

    public static void main(String[] args) {
        String host = (args.length < 1) ? null : args[0];   // null -> localhost
        try {
            Registry registry = LocateRegistry.getRegistry(host);
            GameStateManager gameManager = (GameStateManager) registry.lookup(MANAGER_NAME);
            final String playerId = gameManager.addPlayer();
            final LocalView localView = new LocalView(gameManager, playerId);
            SwingUtilities.invokeLater(() -> localView.setVisible(true));

            final java.util.Timer timer = new Timer(true); // Use daemon thread for timer
            timer.scheduleAtFixedRate(new TimerTask() {
                @Override
                public void run() {
                    // AI movement for p1, p3, p4
                    /*AIMovement.moveAI("p1", gameManager);
                    AIMovement.moveAI("p3", gameManager); // Assuming p3 is AI
                    AIMovement.moveAI("p4", gameManager); // Assuming p4 is AI*/
                    try {
                        var p = gameManager.getWorld().getPlayerById(playerId).get();
                        System.out.println("POS: " + p.getX() + ", " + p.getY());
                    } catch (RemoteException e) {
                        throw new RuntimeException(e);
                    }
                    SwingUtilities.invokeLater(localView::repaintView);
                }
            }, 0, GAME_TICK_MS);
        } catch (Exception e) {
            System.err.println("Client exception: " + e.toString());
            e.printStackTrace();
        }
    }
}