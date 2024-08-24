package com.be.rebook.chat.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.be.rebook.chat.entity.ChatMessage;

public interface ChatMessageRepository extends JpaRepository<ChatMessage, Long> {
}
