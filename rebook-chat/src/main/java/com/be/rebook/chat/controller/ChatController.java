package com.be.rebook.chat.controller;

import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Controller;

import com.be.rebook.chat.dto.ChatMessage;
import com.be.rebook.chat.service.ChatRoomService;
import com.be.rebook.chat.service.RedisPublisher;

@Controller
public class ChatController {

    private final SimpMessagingTemplate messagingTemplate;
    private final RedisPublisher redisPublisher;
    private final ChatRoomService chatRoomService;

    public ChatController(SimpMessagingTemplate messagingTemplate,
                          RedisPublisher redisPublisher,
                          ChatRoomService chatRoomService) {
        this.messagingTemplate = messagingTemplate;
        this.redisPublisher = redisPublisher;
        this.chatRoomService = chatRoomService;
    }

    @MessageMapping("/chat/send")
    public void sendMessage(ChatMessage chatMessage) {
        Long roomId = chatMessage.getRoomId();
        ChannelTopic topic = new ChannelTopic("chatroom." + roomId);
        redisPublisher.publish(topic, chatMessage);
    }

    @MessageMapping("/chat/sub-room")
    public void enterRoom(Long chatRoomId) {
        ChannelTopic topic = new ChannelTopic("chatroom." + chatRoomId);
        chatRoomService.subscribeRoom(topic);
    }
}
