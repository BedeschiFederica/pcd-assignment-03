package pcd.ass03;

import pcd.ass03.model.*;
import pcd.ass03.view.GlobalView;

import javax.swing.*;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.*;
import java.util.Timer;

public class Server {

    private static final String MANAGER_NAME = "manager";
    private static final int WORLD_WIDTH = 1000;
    private static final int WORLD_HEIGHT = 1000;
    private static final int NUM_FOODS = 100;
    private static final long GAME_TICK_MS = 30; // Corresponds to ~33 FPS
    private static final int PORT = 0;

    private static GameStateManager gameManager;
    private static GlobalView globalView;
    private static Registry registry;

    public static void main(String[] args) {

        final List<Food> initialFoods = GameInitializer.initialFoods(NUM_FOODS, WORLD_WIDTH, WORLD_HEIGHT);
        final World initialWorld = new World(WORLD_WIDTH, WORLD_HEIGHT, new ArrayList<>(), initialFoods);
        try {
            gameManager = new DefaultGameStateManager(initialWorld);
            GameStateManager managerStub = (GameStateManager) UnicastRemoteObject.exportObject(gameManager, PORT);
            globalView = new GlobalView(managerStub);
            registry = LocateRegistry.getRegistry();
            registry.rebind(MANAGER_NAME, managerStub);
        } catch (RemoteException e) {
            System.err.println("Server exception in manager init: " + e);
            e.printStackTrace();
        }
        SwingUtilities.invokeLater(() -> globalView.setVisible(true));

        final Timer timer = new Timer(true);
        timer.scheduleAtFixedRate(new TimerTask() {
            @Override
            public void run() {
                try {
                    gameManager.tick();
                    SwingUtilities.invokeLater(globalView::repaintView);
                    checkWinner(timer);
                } catch (final RemoteException | NotBoundException e) {
                    log("Server exception in game: " + e);
                    e.printStackTrace();
                }
            }
        }, 0, GAME_TICK_MS);
    }

    private static void checkWinner(final Timer timer) throws RemoteException, NotBoundException {
        Optional<Player> winner = gameManager.getWinner();
        if (winner.isPresent()) {
            final String winnerId = winner.get().getId();
            for (final WinnerListener l: gameManager.getListeners()) {
                l.setWinner(winnerId);
            }
            timer.cancel();
            registry.unbind(MANAGER_NAME);
            UnicastRemoteObject.unexportObject(gameManager, false);
            SwingUtilities.invokeLater(() -> {
                JOptionPane.showConfirmDialog(globalView, "The winner is " + winnerId, "End game",
                        JOptionPane.DEFAULT_OPTION);
                globalView.dispose();
            });
        }
    }

    private static void log(String msg) {
        System.out.println("[ " + System.currentTimeMillis() + " ][ Main ] " + msg);
    }
}
