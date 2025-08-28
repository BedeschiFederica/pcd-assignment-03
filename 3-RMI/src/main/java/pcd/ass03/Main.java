/*
package pcd.ass03;

import pcd.ass03.model.*;
import pcd.ass03.view.GlobalView;
import pcd.ass03.view.LocalView;

import javax.swing.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

public class Main {

    private static final int WORLD_WIDTH = 1000;
    private static final int WORLD_HEIGHT = 1000;
    private static final int NUM_PLAYERS = 4; // p1, p2, p3, p4
    private static final int NUM_FOODS = 100;
    private static final long GAME_TICK_MS = 30; // Corresponds to ~33 FPS

    public static void main(String[] args) {
        final List<Player> initialPlayers = GameInitializer.initialPlayers(NUM_PLAYERS, WORLD_WIDTH, WORLD_HEIGHT);
        final List<Food> initialFoods = GameInitializer.initialFoods(NUM_FOODS, WORLD_WIDTH, WORLD_HEIGHT);
        final World initialWorld = new World(WORLD_WIDTH, WORLD_HEIGHT, initialPlayers, initialFoods);
        final GameStateManager gameManager = new DefaultGameStateManager(initialWorld);

        // List to keep track of active views for repainting
        final List<JFrameRepaintable> views = new ArrayList<>();

        SwingUtilities.invokeLater(() -> {
            GlobalView globalView = new GlobalView(gameManager);
            views.add(globalView::repaintView); // Add repaint method reference
            globalView.setVisible(true);

            LocalView localViewP1 = new LocalView(gameManager, "p1");
            views.add(localViewP1::repaintView);
            localViewP1.setVisible(true);

            LocalView localViewP2 = new LocalView(gameManager, "p2");
            views.add(localViewP2::repaintView);
            localViewP2.setVisible(true);
        });

        final Timer timer = new Timer(true); // Use daemon thread for timer
        timer.scheduleAtFixedRate(new TimerTask() {
            @Override
            public void run() {
                // AI movement for p1, p3, p4
                AIMovement.moveAI("p1", gameManager);
                AIMovement.moveAI("p3", gameManager); // Assuming p3 is AI
                AIMovement.moveAI("p4", gameManager); // Assuming p4 is AI

                gameManager.tick();

                SwingUtilities.invokeLater(() -> {
                    for (JFrameRepaintable view : views) {
                        view.repaintView();
                    }
                });
            }
        }, 0, GAME_TICK_MS);
    }

    // Functional interface for repaintable views
    @FunctionalInterface
    interface JFrameRepaintable {
        void repaintView();
    }
}
*/
