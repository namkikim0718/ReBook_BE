package com.be.rebook.chat.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.data.redis.listener.adapter.MessageListenerAdapter;

import com.be.rebook.chat.service.RedisSubscriber;

@Configuration
public class RedisConfig {

    @Bean
    RedisMessageListenerContainer redisContainer(RedisConnectionFactory connectionFactory, 
                                                 MessageListenerAdapter listenerAdapter) {
        RedisMessageListenerContainer container = new RedisMessageListenerContainer();
        container.setConnectionFactory(connectionFactory);
        return container;
    }

    @Bean
    MessageListenerAdapter listenerAdapter(RedisSubscriber redisSubscriber) {
        return new MessageListenerAdapter(redisSubscriber, "handleMessage");
    }
}
