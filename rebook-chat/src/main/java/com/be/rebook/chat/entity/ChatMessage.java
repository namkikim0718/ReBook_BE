package com.be.rebook.chat.entity;

import com.be.rebook.chat.dto.ChatMessageDTO;
import com.be.rebook.common.config.BaseEntity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Entity
@Getter
@NoArgsConstructor
public class ChatMessage extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "chat_message_id")
    private Long id;

    @Column(name = "message")
    private String message;

    @Column(name = "sender_id")
    private Long senderId;

    @Column(name = "is_read")
    private Boolean isRead;

    @ManyToOne
    @JoinColumn(name = "chat_room_id")
    private ChatRoom chatRoom;

    public ChatMessage(ChatMessageDTO message, ChatRoom chatRoom) {
        this.message = message.getMessage();
        this.senderId = message.getSenderId();
        this.isRead = false;
        this.chatRoom = chatRoom;
    }
}
