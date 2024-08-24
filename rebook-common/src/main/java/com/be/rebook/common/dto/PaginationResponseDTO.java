package com.be.rebook.common.dto;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Page;

import java.util.List;

@Getter
@Setter
public class PaginationResponseDTO<T> {

    private int currentPage;
    private int totalPages;
    private long totalElements;
    private int size;
    private List<T> content;

    public PaginationResponseDTO(Page<T> page) {
        this.currentPage = page.getNumber();
        this.totalPages = page.getTotalPages();
        this.totalElements = page.getTotalElements();
        this.size = page.getSize();
        this.content = page.getContent();
    }
}