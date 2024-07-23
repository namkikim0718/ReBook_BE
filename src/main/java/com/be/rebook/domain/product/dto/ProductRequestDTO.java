package com.be.rebook.domain.product.dto;

import lombok.Builder;
import lombok.Getter;

public class ProductRequestDTO {

    @Getter
    @Builder
    public static class ProductRequest {
        private String title;

        private String content;

        private int price;

        private String isbn;

        private String bookTitle;

        private String author;

        private String publisher;

        private String publishDate;

        private String university;

        private String major;
    }
}
