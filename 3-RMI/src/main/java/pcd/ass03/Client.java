package pcd.ass03;

import pcd.ass03.model.AIMovement;
import pcd.ass03.model.GameStateManager;
import pcd.ass03.model.WinnerListener;
import pcd.ass03.model.WinnerListenerImpl;
import pcd.ass03.view.LocalView;

import javax.swing.*;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.Timer;
import java.util.TimerTask;

public class Client {

	private static final String MANAGER_NAME = "manager";
    private static final long GAME_TICK_MS = 30; // Corresponds to ~33 FPS

    static GameStateManager gameManager;
    static String playerId;
    static LocalView localView;
    static final WinnerListener winnerListener = new WinnerListenerImpl();

    public static void main(String[] args) {
        final boolean isAi = args.length > 0 && args[0].equals("ai");
        final String host = (args.length < 2) ? null : args[1];   // null -> localhost
        final Registry registry;
        try {
            registry = LocateRegistry.getRegistry(host);
            gameManager = (GameStateManager) registry.lookup(MANAGER_NAME);
            playerId = gameManager.addPlayer();
            var l = (WinnerListener) UnicastRemoteObject.exportObject(winnerListener, 0);
            gameManager.addListener(l);
        } catch (final RemoteException | NotBoundException e) {
            System.err.println("Client exception in manager init: " + e);
            e.printStackTrace();
        }
        localView = new LocalView(gameManager, playerId);
        SwingUtilities.invokeLater(() -> localView.setVisible(true));

        final Timer timer = new Timer(true); // Use daemon thread for timer
        timer.scheduleAtFixedRate(new TimerTask() {
            @Override
            public void run() {
                try {
                    if (gameManager.getWorld().getPlayerById(playerId).isEmpty()) {
                        localView.dispose();
                        timer.cancel();
                    }
                    if (isAi) {
                        AIMovement.moveAI(playerId, gameManager);
                    }
                    SwingUtilities.invokeLater(localView::repaintView);
                } catch (final RemoteException | RuntimeException e) {
                    localView.dispose();
                    timer.cancel();
                    checkWinner(e);
                }
            }
        }, 0, GAME_TICK_MS);
    }

    private static void checkWinner(final Exception e) {
        try {
            if (winnerListener.getWinnerId().isPresent()) {
                System.out.println("WINNER: " + winnerListener.getWinnerId().get());
            } else {
                System.err.println("Client exception in game: " + e);
                disconnectPlayer();
            }
            UnicastRemoteObject.unexportObject(winnerListener, false);
        } catch (final RemoteException ex) {
            System.err.println("Client exception in checking winner: " + ex);
            ex.printStackTrace();
        }
    }

    private static void disconnectPlayer() {
        try {
            gameManager.removePlayer(playerId);
            gameManager.removeListener(winnerListener);
        } catch (final RemoteException | RuntimeException e) {
            System.err.println("Client exception in player disconnection: " + e);
            e.printStackTrace();
        }
    }
}
