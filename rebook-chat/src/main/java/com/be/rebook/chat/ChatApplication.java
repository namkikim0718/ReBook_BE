package com.be.rebook.chat;

import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;

import com.corundumstudio.socketio.SocketIOServer;

@EnableJpaAuditing
@SpringBootApplication
@EnableDiscoveryClient
@ComponentScan(basePackages = {"com.be.rebook.common", "com.be.rebook.chat"})
public class ChatApplication {
    public static void main(String[] args) {
        // Spring Boot 애플리케이션 실행
        SpringApplication.run(ChatApplication.class, args);
    }

    @Bean
    public CommandLineRunner runner(SocketIOServer server) {
        return args -> {
            server.start();
            Runtime.getRuntime().addShutdownHook(new Thread(server::stop));
        };
    }
}
