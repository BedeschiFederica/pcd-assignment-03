package pcd.ass03;

import pcd.ass03.model.AIMovement;
import pcd.ass03.model.GameStateManager;
import pcd.ass03.view.LocalView;

import javax.swing.*;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.Timer;
import java.util.TimerTask;

public class Client {

	private static final String MANAGER_NAME = "manager";
    private static final long GAME_TICK_MS = 30; // Corresponds to ~33 FPS

    public static void main(String[] args) {
        final boolean isAi = args.length > 0 && args[0].equals("ai");
        final String host = (args.length < 2) ? null : args[1];   // null -> localhost
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
                    if (isAi) {
                        try {
                            AIMovement.moveAI(playerId, gameManager);
                        } catch (final RemoteException e) {
                            throw new RuntimeException(e);
                        }
                    }
                    SwingUtilities.invokeLater(localView::repaintView);
                }
            }, 0, GAME_TICK_MS);
        } catch (final Exception e) {
            System.err.println("Client exception: " + e);
            e.printStackTrace();
        }
    }
}